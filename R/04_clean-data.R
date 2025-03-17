#-------------------------------------------------------------------------------
# Script: Filter Acoustic Detections based on Time Proximity
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-01
#
# Description:
# Filters acoustic detection data by removing isolated detections that occur
# only once at a given location within a specified time window, following the
# methodology by Kessel et al. (2014) to remove potential spurious detections.
#
# Dependencies:
# - tidyverse
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse)

# Function Definitions

#-------------------------------------------------------------------------------
# Function: validate_detection_data
#-------------------------------------------------------------------------------
#' @description Validate detection data structure
#' @param det Data frame containing detection data
#' @throws Error if required columns are missing
#' @return Invisibly returns TRUE if validation passes
validate_detection_data <- function(det) {
  required_cols <- c("elasmo", "location", "time")
  missing_cols <- setdiff(required_cols, names(det))

  if (length(missing_cols) > 0) {
    stop("Input data missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!inherits(det$time, "POSIXct")) {
    stop("Time column must be POSIXct datetime format")
  }

  invisible(TRUE)
}

#-------------------------------------------------------------------------------
# Function: flag_single_detections
#-------------------------------------------------------------------------------
#' @description Flag isolated detections at a location within time window
#' @param det Data frame containing detection data
#' @param time_window_hours Time window in hours (default = 1)
#' @return Data frame with added 'single_detection' flag column
#' @examples
#' \dontrun{
#' flagged_data <- flag_single_detections(detections, time_window_hours = 2)
#' }
flag_single_detections <- function(det, time_window_hours = 1) {
  # Input validation
  validate_detection_data(det)
  if (!is.numeric(time_window_hours) || time_window_hours <= 0) {
    stop("time_window_hours must be a positive numeric value")
  }

  det %>%
    arrange(elasmo, location, time) %>%
    group_by(elasmo, location) %>%
    mutate(
      time_next = lead(time),
      time_prev = lag(time),
      time_diff_to_next = as.numeric(
        difftime(time_next, time, units = "hours")
      ),
      time_diff_to_prev = as.numeric(
        difftime(time, time_prev, units = "hours")
      ),
      single_detection = as.integer(
        (is.na(time_diff_to_next) | time_diff_to_next > time_window_hours) &
          (is.na(time_diff_to_prev) | time_diff_to_prev > time_window_hours)
      )
    ) %>%
    select(-time_next, -time_prev) %>%
    ungroup()
}

#-------------------------------------------------------------------------------
# Function: remove_single_detections
#-------------------------------------------------------------------------------
#' @description Remove flagged single detections
#' @param det Data frame with 'single_detection' column
#' @param keep_flags Keep flag columns in output? (default = FALSE)
#' @return Cleaned data frame with single detections removed
remove_single_detections <- function(det, keep_flags = FALSE) {
  if (!"single_detection" %in% names(det)) {
    stop("Input data must contain 'single_detection' column")
  }

  cleaned <- det %>%
    filter(single_detection == 0) %>%
    arrange(time)

  if (!keep_flags) {
    cleaned <- cleaned %>%
      select(-single_detection, -starts_with("time_diff"))
  }

  return(cleaned)
}

#-------------------------------------------------------------------------------
# Function: calculate_removal_stats
#-------------------------------------------------------------------------------
#' @description Calculate detection removal statistics
#' @param original_count Original number of detections
#' @param cleaned_count Number after removal
#' @return Invisible list with removal statistics
calculate_removal_stats <- function(original_count, cleaned_count) {
  stats <- list(
    removed = original_count - cleaned_count,
    remaining = cleaned_count,
    removal_pct = (1 - (cleaned_count / original_count)) * 100
  )

  cat(sprintf(
    paste0(
      "Detection Filtering Results:\n",
      "---------------------------\n",
      "Original detections: %d\n",
      "Remaining detections: %d\n",
      "Removed detections: %d (%.2f%%)\n"
    ),
    original_count, cleaned_count, stats$removed, stats$removal_pct
  ))

  invisible(stats)
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------
# Global Configuration
config <- list(
  input_file = "det_tot.rds",
  output_file = "det_cleaned.rds",
  time_window_hours = 1,
  data_directory = here::here("data")
)

# Load Data with error handling
tryCatch({
  det <- readRDS(file.path(config$data_directory, config$input_file))
}, error = function(e) {
  stop("Failed to load detection data: ", e$message)
})

# Processing pipeline
processing_result <- tryCatch({
  det %>%
    flag_single_detections(time_window_hours = config$time_window_hours) %>%
    remove_single_detections()
}, error = function(e) {
  message("Processing failed: ", e$message)
  return(NULL)
})

if (!is.null(processing_result)) {
  # Save results
  saveRDS(processing_result, file.path(config$data_directory, config$output_file))

  # Generate statistics
  calculate_removal_stats(nrow(det), nrow(processing_result))
  cat("Detection filtering complete. Cleaned data saved to", config$output_file, "\n")
} else {
  message("No output generated due to processing errors")
}
