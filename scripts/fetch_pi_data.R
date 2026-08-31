# Load necessary libraries
# install.packages(c("httr", "jsonlite", "ggplot2", "dplyr", "lubridate"))
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)

# --- Configuration ---
PI_WEB_API_BASE_URL <- "https://hinuta105.ccit.ad.sharedom.net/piwebapi"
PI_SERVER_NAME <- "HINUTA101C"
TAG_NAMES <- c("M2TI4405.PV", "M2PI4407.PV", "M2PI4410W.PV", "M2PI4411W.PV", "M2FC4403.PV", "M2CI2901.CPV")

# Authentication
if (file.exists("auth_config.R")) {
  source("auth_config.R")
  auth_config <- authenticate(AUTH_USER, AUTH_PASS)
} else {
  stop("Authentication configuration file 'auth_config.R' is missing. Please copy 'auth_config_template.R' to 'auth_config.R' and set your credentials.")
}

# Disable SSL verification for internal servers (optional, use with caution)
ssl_config <- config(ssl_verifypeer = 0)

# --- Functions ---

#' Get WebID for a specific PI Tag
#' @param tag_name The name of the PI Point
#' @return The WebId string
get_web_id <- function(tag_name) {
  # Construct the path: \\ServerName\TagName
  path <- sprintf("\\\\%s\\%s", PI_SERVER_NAME, tag_name)

  url <- paste0(PI_WEB_API_BASE_URL, "/points")

  response <- GET(
    url,
    query = list(path = path),
    auth_config,
    ssl_config
  )

  if (status_code(response) != 200) {
    warning(paste("Failed to get WebID for", tag_name, ":", content(response, "text")))
    return(NULL)
  }

  content_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(content_json$WebId)
}

#' Get Recorded Data for a WebID
#' @param web_id The WebId of the stream
#' @param start_time Start time string (e.g., "*-3d")
#' @param end_time End time string (e.g., "*")
#' @return A data frame of the recorded values
get_recorded_data <- function(web_id, tag_name, start_time = "*-3d", end_time = "*") {
  url <- paste0(PI_WEB_API_BASE_URL, "/streams/", web_id, "/recorded")

  response <- GET(
    url,
    query = list(startTime = start_time, endTime = end_time),
    auth_config,
    ssl_config
  )

  if (status_code(response) != 200) {
    warning(paste("Failed to get data for", tag_name, ":", content(response, "text")))
    return(NULL)
  }

  content_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
  items <- content_json$Items

  if (is.null(items) || length(items) == 0) {
    return(NULL)
  }

  # Handle complex value (e.g. Digital State object or System Error)
  vals <- items$Value
  
  if (is.data.frame(vals)) {
    # If it's a data frame, we want the "Value" column if it exists, otherwise the first column.
    # We must NOT unlist the entire data frame as that concatenates columns.
    if ("Value" %in% names(vals)) {
      vals <- vals$Value
    } else {
      vals <- vals[[1]]
    }
  } else if (is.list(vals)) {
    # If it's a list of objects (e.g. from JSON), extract the Value field or the first element
    # Use sapply to ensure we get a vector of the same length, handling NULLs
    vals <- sapply(vals, function(x) {
      if (is.list(x) || is.data.frame(x)) {
         if ("Value" %in% names(x)) return(x$Value)
         if (length(x) > 0) return(x[[1]])
         return(NA)
      } else {
         return(x)
      }
    })
  }

  # Select relevant columns and add TagName
  df <- data.frame(
    Timestamp = items$Timestamp,
    Value = vals,
    Tag = tag_name,
    stringsAsFactors = FALSE
  )

  return(df)
}

# --- Main Execution ---

all_data <- data.frame()

message("Starting data retrieval...")

for (tag in TAG_NAMES) {
  message(paste("Processing:", tag))

  web_id <- get_web_id(tag)

  if (!is.null(web_id)) {
    tag_data <- get_recorded_data(web_id, tag)

    if (!is.null(tag_data)) {
      all_data <- rbind(all_data, tag_data)
    }
  }
}

# Data Cleaning
if (nrow(all_data) > 0) {
  # Convert Timestamp to POSIXct
  all_data$Timestamp <- ymd_hms(all_data$Timestamp)

  # Convert Value to numeric (handling digital states or errors which might be lists or strings)
  # Note: PI Web API might return a nested object for Value if it's a system state.
  # Simple coercion:
  all_data$Value <- as.numeric(all_data$Value)

  # Filter out NAs created by coercion if necessary
  all_data <- all_data %>% filter(!is.na(Value))

  # --- Plotting ---
  p <- ggplot(all_data, aes(x = Timestamp, y = Value, color = Tag)) +
    geom_line() +
    labs(title = "PI Data Trends",
         subtitle = paste(TAG_NAMES, collapse = ", "),
         x = "Time",
         y = "Value") +
    theme_minimal()

  print(p)

  # Save the plot
  ggsave("pi_data_plot.png", plot = p, width = 10, height = 6)
  message("Plot saved to pi_data_plot.png")

} else {
  warning("No data retrieved.")
}
