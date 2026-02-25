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

# --- Functions ---

#' Get WebID for a specific PI Tag
#' @param tag_name The name of the PI Point
#' @return The WebId string
get_web_id <- function(tag_name) {
  if (is.null(auth_config)) {
    warning("No authentication config loaded.")
    return(NULL)
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
#' @param start_time Start time string (e.g., "*-3d")
#' @param end_time End time string (e.g., "*")
#' @return A data frame of the recorded values
get_recorded_data <- function(web_id, tag_name, start_time = "*-3d", end_time = "*") {
  if (is.null(auth_config)) {
    warning("No authentication config loaded.")
    return(NULL)
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
