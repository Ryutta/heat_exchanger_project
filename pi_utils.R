library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# --- Configuration ---
PI_WEB_API_BASE_URL <- "https://hinuta105.ccit.ad.sharedom.net/piwebapi"
PI_SERVER_NAME <- "HINUTA101C"

# Authentication
if (file.exists("auth_config.R")) {
  source("auth_config.R")
  if (exists("AUTH_USER") && exists("AUTH_PASS")) {
    auth_config <- authenticate(AUTH_USER, AUTH_PASS)
  } else {
    warning("AUTH_USER or AUTH_PASS not found in auth_config.R")
    auth_config <- NULL
  }
} else {
  warning("Authentication configuration file 'auth_config.R' is missing.")
  auth_config <- NULL
}

# Disable SSL verification for internal servers
ssl_config <- config(ssl_verifypeer = 0)

# Mock Data Generation (if API fails or no auth)
generate_mock_data <- function(tag_name, start_time, end_time) {
  # Mock data range covering some periods defined in Analysis_1st.Rmd
  start_dt <- as.POSIXct("2021-02-01 00:00:00")
  end_dt <- as.POSIXct("2024-04-12 00:00:00")
  timestamps <- seq(start_dt, end_dt, by = "hour")

  # Basic mock values based on tag type
  if (grepl("Flow", tag_name) || grepl("FC", tag_name)) {
    vals <- runif(length(timestamps), 2300, 2400)
  } else if (grepl("TI", tag_name)) { # Temp
    vals <- runif(length(timestamps), 20, 30)
  } else if (grepl("PI", tag_name) && grepl("10W", tag_name)) { # Jacket P
    vals <- runif(length(timestamps), 0, 100) # kPa gauge?
  } else if (grepl("PI", tag_name) && grepl("11W", tag_name)) { # Pump Outlet
    vals <- runif(length(timestamps), 300, 400) # kPa gauge
  } else if (grepl("PI", tag_name)) { # Pump Inlet
    vals <- runif(length(timestamps), 50, 80) # kPa gauge
  } else {
    vals <- runif(length(timestamps), 0, 100)
  }

  data.frame(
    Timestamp = timestamps,
    Value = vals,
    Tag = tag_name,
    stringsAsFactors = FALSE
  )
}

# --- Functions ---

#' Get WebID for a specific PI Tag
#' @param tag_name The name of the PI Point
#' @return The WebId string
get_web_id <- function(tag_name) {
  if (is.null(auth_config)) {
    warning("No authentication config loaded. Using mock WebID.")
    return(paste0("mock_webid_", tag_name))
  }

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
#' @param tag_name The name of the tag (for mock generation)
#' @param start_time Start time string (e.g., "*-3d")
#' @param end_time End time string (e.g., "*")
#' @return A data frame of the recorded values
get_recorded_data <- function(web_id, tag_name, start_time = "*-3d", end_time = "*") {
  if (is.null(auth_config) || grepl("mock_webid", web_id)) {
    warning("No authentication config loaded or mock WebID used. Returning mock data.")
    return(generate_mock_data(tag_name, start_time, end_time))
  }

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

  # Handle complex value
  vals <- items$Value

  if (is.data.frame(vals)) {
    if ("Value" %in% names(vals)) {
      vals <- vals$Value
    } else {
      vals <- vals[[1]]
    }
  } else if (is.list(vals)) {
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

  df <- data.frame(
    Timestamp = items$Timestamp,
    Value = vals,
    Tag = tag_name,
    stringsAsFactors = FALSE
  )

  # Clean timestamp and value
  df$Timestamp <- ymd_hms(df$Timestamp)
  df$Value <- as.numeric(df$Value)
  df <- df %>% filter(!is.na(Value))

  return(df)
}
