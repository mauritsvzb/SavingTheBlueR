#-------------------------------------------------------------------------------
# Script: Filter Acoustic Detections based on Time Proximity
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-01
#
# Description:
# This script filters acoustic detection data by removing detections that
# occur only once at a given location within a one-hour window, based
# on the criteria used by Kessel et al. (2014), to remove potential spurious
# detections.
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
# Function: flag_single_detections
#-------------------------------------------------------------------------------
#' @description Flags detections that occur only once at a given location
#' within a specified time window.
#' @param det Data frame containing detection data.
#' @param time_window_hours The time window in hours.
#' @return A data frame with a new column (`single_detection`) indicating
#' detections occurring only once within the time window.
flag_single_detections <- function(det, time_window_hours = 1) {
  det %>%
    group_by(elasmo, location) %>%
    mutate(
      time_next = lead(time),
      time_prev = lag(time),
      time_diff_to_next = as.numeric(difftime(time_next, time, units = "hours")),
      time_diff_to_prev = as.numeric(difftime(time, time_prev, units = "hours")),
      single_detection = ifelse(
        (is.na(time_diff_to_next) | time_diff_to_next > time_window_hours) &
          (is.na(time_diff_to_prev) | time_diff_to_prev > time_window_hours),
        1, 0
      )
    ) %>%
    select(-time_next, -time_prev) %>%
    ungroup()
}

#-------------------------------------------------------------------------------
# Function: remove_single_detections
#-------------------------------------------------------------------------------
#' @description Removes detections flagged as single detections (occurring only
#' once within the specified time window).
#' @param det Data frame containing detection data with the `single_detection`
#' column.
#' @return A cleaned data frame with single detections removed.
remove_single_detections <- function(det) {
  det %>%
    filter(single_detection == 0) %>% # Keep only detections that are NOT single
    select(-single_detection, -time_diff_to_next, -time_diff_to_prev) %>% # Remove the flag columns
    arrange(time) # Sort by time
}

#-------------------------------------------------------------------------------
# Function: calculate_removal_stats
#-------------------------------------------------------------------------------
#' @description Calculates and prints statistics about the number and percentage
#' of removed detections.
#' @param original_count Number of detections in the original data frame.
#' @param cleaned_count Number of detections in the cleaned data frame.
calculate_removal_stats <- function(original_count, cleaned_count) {
  removed_count <- original_count - cleaned_count
  removed_percentage <- (1 - (cleaned_count / original_count)) * 100

  cat("Removed spurious detections:", removed_count, "\n")
  cat("Removed spurious detections percentage:", removed_percentage, "%\n")
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------

# Define Global Variables/Constants
data_directory <- here::here("data")

# Load the detection data
det <- readRDS(file.path(data_directory, "DET_tot.rds"))

# Order the detection data
ordered_det <- det %>%
  arrange(elasmo, location, time)

# Flag detections that occur only once within a 1-hour time window
preclean <- flag_single_detections(ordered_det)

# Remove detections flagged as single detections
cleaned_df <- remove_single_detections(preclean)

# Calculate and print removal statistics
calculate_removal_stats(nrow(ordered_det), nrow(cleaned_df))

# Save the cleaned detection data
saveRDS(cleaned_df, file.path(data_directory, "DET_cleaned.rds"))

cat("Detection filtering complete. Cleaned data saved.\n")
