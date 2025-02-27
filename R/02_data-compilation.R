#-------------------------------------------------------------------------------
# Script: Compile and Filter Acoustic Detection Data
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-02-24
#
# Description:
# This script compiles and filters acoustic detection data by:
# 1. Matching detections to tagged animals
# 2. Verifying detections occurred during receiver deployment periods
# 3. Ensuring detections are after tag deployment dates
#
# Dependencies:
# - tidyverse
# - lubridate
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, lubridate)

# Define Global Variables/Constants
data_timezone <- "US/Eastern"
data_directory <- here::here("data")

# Function Definitions

#-------------------------------------------------------------------------------
# Function: import_and_preprocess_data
#-------------------------------------------------------------------------------
#' @description Imports and preprocesses detection data, receiver movement data, and individual data
#' @param datadir Directory containing the data files
#' @param timezone The timezone for the data
#' @return A list containing the preprocessed dataframes
import_and_preprocess_data <- function(datadir, timezone) {
  # Import detection data
  det <- readRDS(file.path(datadir, "DET.rds")) %>%
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
        with_tz(timezone),
      station = as.character(station),
      elasmo = as.character(elasmo)
    ) %>%
    arrange(elasmo, time)

  # Import receiver movement data
  VMOV <- readRDS(file.path(datadir, "VMOV.rds"))

  # Import individual data
  IND <- readRDS(file.path(datadir, "IND.rds"))

  list(det = det, VMOV = VMOV, IND = IND)
}

#-------------------------------------------------------------------------------
# Function: filter_detections_by_tag_and_tag_deployment
#-------------------------------------------------------------------------------
#' @description Filter to remove unknown tags and detections that may have been biased by tagging event
#' @param det Detection dataframe
#' @param IND Individual dataframe
#' @param filter Boolean to indicate if data collected during first 24 hours post tag deployment should be removed
#' @return Filtered detection dataframe
filter_detections_by_tag_deployment <- function(det, IND, filter = TRUE) {
  if (filter) {
    IND <- IND %>% mutate(tagging_datetime = tagging_datetime + hours(24))
  }

  det %>%
    semi_join(IND, by = c("elasmo" = "acoustic_tag_id")) %>%
    filter(time >= IND$tagging_datetime[match(elasmo, IND$acoustic_tag_id)])
}

#-------------------------------------------------------------------------------
# Function: assign_locations_to_detections
#-------------------------------------------------------------------------------
#' @description Assigns locations to detections based on receiver deployment periods
#' @param det Detection dataframe
#' @param VMOV Receiver movement dataframe
#' @return Detection dataframe with assigned locations
assign_locations_to_detections <- function(det, VMOV) {
  {suppressWarnings(left_join( #suppressWarnings() suppresses false alarm warnings originating from
                               #the many duplicates that are created by the left_join(), which are
                               #dealt with using the filter()
    .,
    VMOV %>%
      select(`Receiver ID`, STATION_NO, `Date In`, `Date Out`) %>%
      rename(station = `Receiver ID`, location = STATION_NO),
    by = "station"
  ))} %>%
    filter(time >= `Date In`, time <= `Date Out`) %>%
    select(-`Date In`, -`Date Out`)
}

#-------------------------------------------------------------------------------
# Function: compile_data
#-------------------------------------------------------------------------------
#' @description Main function to compile and filter acoustic detection data
#' @param datadir Directory containing the data files
#' @param timezone Timezone for the data
#' @param filter Boolean to indicate if first 24 hours should be filtered out
#' @return Compiled and filtered detection dataframe
compile_data <- function(datadir, timezone, filter = FALSE) {
  # Import and preprocess data
  data <- import_and_preprocess_data(datadir, timezone)

  # Filter detections by tag deployment
  filtered_det <- filter_detections_by_tag_deployment(data$det, data$IND, filter)

  # Assign locations to detections
  compiled_det <- assign_locations_to_detections(filtered_det, data$VMOV)

  # Remove detections without assigned locations
  compiled_det <- compiled_det %>% filter(!is.na(location))

  # Save compiled data
  saveRDS(compiled_det, file.path(datadir, "DET_compiled.rds"))

  compiled_det
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------

# Run the compile function
compiled_data <- compile_data(data_directory, data_timezone, filter = FALSE)

# Print summary of compiled data
cat("Compilation complete. Summary of compiled data:\n")
print(summary(compiled_data))
