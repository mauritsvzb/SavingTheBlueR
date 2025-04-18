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

# Fresh Start # Un-comment below code if you want to run in this script
# rm(list = ls())

# Load Libraries # Un-comment below code if you want to run in this script
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(here, tidyverse)

# Global Configuration # Un-comment below code if you want to run in this script
# config <- list(
#   data_timezone = "US/Eastern",
#   data_directory = here::here("data")
# )

# Function Definitions

#-------------------------------------------------------------------------------
# Function: import_and_preprocess_data
#-------------------------------------------------------------------------------
#' @description Imports and preprocesses detection data, receiver movement data,
#' and individual data.
#' @param config_list List of configuration parameters.
#' @param data_directory Directory containing the data files.
#' @param timezone The timezone for the data.
#' @return A list containing the preprocessed dataframes.
import_and_preprocess_data <- function(config_list, data_directory, timezone) {
  tryCatch({
    # Import detection data
    det <- readRDS(file.path(data_directory, "det.rds")) %>%
      mutate(
        time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
          with_tz(timezone),
        station = as.character(station),
        elasmo = as.character(elasmo)
      ) %>%
      arrange(elasmo, time)

    # Import receiver movement data
    vmov <- readRDS(file.path(data_directory, "vmov.rds"))

    # Import individual data
    ind <- readRDS(file.path(data_directory, "ind.rds"))

    list(det = det, vmov = vmov, ind = ind)
  }, error = function(e) {
    cat("Error importing and preprocessing data:", e$message, "\n")
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: filter_detections_by_tag_and_tag_deployment
#-------------------------------------------------------------------------------
#' @description Filter to remove unknown tags and detections that may have been
#' biased by tagging event.
#' @param config_list List of configuration parameters.
#' @param det Detection dataframe.
#' @param ind Individual dataframe.
#' @param filter_24h Boolean to indicate if data collected during first 24 hours post
#' tagging should be removed, but also removes detections that occurred prior to
#' tag deployment and no detection thereafter.
#' @return Filtered detection dataframe.
filter_detections_by_tag_and_tag_deployment <- function(config_list, det, ind, filter_24h = FALSE) {

  # Check and convert data types if necessary
  if (is.character(det$elasmo) && is.numeric(ind$acoustic_tag_id)) {
    det$elasmo <- as.numeric(det$elasmo)
    # cat("Converted 'elasmo' in det to numeric.\n")
  } else if (is.numeric(det$elasmo) && is.character(ind$acoustic_tag_id)) {
    ind$acoustic_tag_id <- as.character(ind$acoustic_tag_id)
    # cat("Converted 'acoustic_tag_id' in IND to character.\n")
  }

  # Find the unique IDs that are in det but not in ind
  excluded_ids <- setdiff(unique(det$elasmo), ind$acoustic_tag_id)

  # Print the excluded IDs
  if (length(excluded_ids) > 0) {
    cat("The following unique transmitter IDs were excluded because they do not occur in the catch database:\n")
    print(excluded_ids)
  } else {
    cat("No transmitter IDs were excluded.\n")
  }

  if (filter_24h) {
    ind <- ind %>% mutate(tagging_datetime = tagging_datetime + hours(24))
  }

  # Perform the semi_join
  det_joined <- det %>%
    semi_join(ind, by = c("elasmo" = "acoustic_tag_id"))

  # Perform the filtering and capture excluded tags
  filtered_det <- det_joined %>%
    filter(time >= ind$tagging_datetime[match(elasmo, ind$acoustic_tag_id)])

  # Find tags excluded by the filter
  excluded_by_filter <- setdiff(unique(det_joined$elasmo), unique(filtered_det$elasmo))

  # Print the tags excluded by the filter
  if (length(excluded_by_filter) > 0) {
    cat("\nThe following tags were removed by the time filter:\n")
    print(excluded_by_filter)
  } else {
    cat("\nNo tags were removed by the time filter.\n")
  }

  return(filtered_det)
}

#-------------------------------------------------------------------------------
# Function: assign_locations_to_detections
#-------------------------------------------------------------------------------
#' @description Assigns locations to detections based on receiver deployment
#' periods.
#' @param config_list List of configuration parameters.
#' @param det Detection dataframe.
#' @param vmov Receiver movement dataframe.
#' @return Detection dataframe with assigned locations.
assign_locations_to_detections <- function(config_list, det, vmov) {
  suppressWarnings( #suppressWarnings() suppresses false alarm warnings originating from
    #the many duplicates that are created by the left_join(), which are
    #dealt with using the filter()
    det %>%
      left_join(
        vmov %>%
          select(station, location, date_in, date_out),
          by = "station"
      )
  ) %>%
    filter(time >= date_in, time <= date_out) %>%
    select(-date_in, -date_out)
}

#-------------------------------------------------------------------------------
# Function: compile_data
#-------------------------------------------------------------------------------
#' @description Main function to compile and filter acoustic detection data.
#' @param config_list List of configuration parameters.
#' @param data_directory Directory containing the data files.
#' @param timezone Timezone for the data.
#' @param filter_24h Boolean to indicate if first 24 hours should be filtered out.
#' @return Compiled and filtered detection dataframe.
compile_data <- function(config_list, data_directory, timezone, filter_24h = FALSE) {
  tryCatch({
    # Import and preprocess data
    data <- import_and_preprocess_data(config_list, data_directory, timezone)

    if (is.null(data)) {
      stop("Data import and preprocessing failed.")
    }

    # Filter detections by tag deployment
    filtered_det <- filter_detections_by_tag_and_tag_deployment(config_list, data$det, data$ind, filter_24h)

    # Assign locations to detections
    compiled_det <- assign_locations_to_detections(config_list, filtered_det, data$vmov)

    # Remove detections without assigned locations
    compiled_det <- compiled_det %>% filter(!is.na(location))

    # Save compiled data
    saveRDS(compiled_det, file.path(data_directory, "det_compiled.rds"))

    return(compiled_det)
  }, error = function(e) {
    cat("Error in compile_data:", e$message, "\n")
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: process_all_data
#-------------------------------------------------------------------------------
#' @description Main processing function for acoustic telemetry pipeline. It imports,
#' preprocesses, filters, assigns locations to detections, and saves compiled results.
#' @param config_list List containing configuration parameters.
#' @return None. The function saves processed data to RDS files as a side effect.
process_all_data <- function(config_list){
  tryCatch({
    # Run the compile function
    compiled_data <- compile_data(config_list,
                 data_directory = config_list$data_directory,
                 timezone = config_list$data_timezone,
                 filter_24h = TRUE)
  }, error = function(e) {
    stop("Data compilation failed: ", e$message)
  })
}
