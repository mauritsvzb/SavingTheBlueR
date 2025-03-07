#-------------------------------------------------------------------------------
# Script: Generate Acoustic Telemetry Data Summaries
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-07
#
# Description:
# This script generates summaries of acoustic detection data including:
# - Individual animal movement statistics
# - Detection matrices by location/agency
# - Temporal analysis of detection patterns
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

# Function Definitions

#-------------------------------------------------------------------------------
# Function: create_time_intervals
#-------------------------------------------------------------------------------
#' @description Creates time intervals for analysis.
#' @param det_data Detection data frame.
#' @param start_point Optional start date (POSIXct).
#' @param time_interval Use monthly intervals (TRUE/FALSE).
#' @return List containing time sequence and interval information
create_time_intervals <- function(det_data, start_point = NULL, time_interval = FALSE) {
  if (time_interval) {
    first_date <- start_point %||% min(det_data$time)
    month_seq <- seq(
      from = floor_date(first_date, "month"),
      to = ceiling_date(max(det_data$time), "month"),
      by = "month"
    )
    list(timeseq = month_seq, interval_type = "monthly")
  } else {
    list(
      timeseq = c(min(det_data$time), max(det_data$time) + minutes(1)),
      interval_type = "full_period"
    )
  }
}

#-------------------------------------------------------------------------------
# Function: generate_individual_summary
#-------------------------------------------------------------------------------
#' @description Generates summary statistics for individual animals.
#' @param filtered_data Filtered detection data for specific time interval.
#' @param ind_data Individual metadata.
#' @param tz Timezone for the individual metadata.
#' @return Tibble with individual summary statistics.
generate_individual_summary <- function(filtered_data, ind_data, timezone) {
  if (nrow(filtered_data) == 0) {
    return(tibble(
      ID = character(), Species = character(), `Date tagged` = character(),
      Sex = character(), PCL = numeric(), FL = numeric(), TL = numeric(), STL = numeric(),
      `Total detections` = integer(), `Total Stations detected` = integer(),
      `Total days detected` = integer(), `First appearance` = character(),
      `Last appearance` = character(), `Days at liberty` = numeric(),
      `Residency Index` = numeric()
    ))
  }

  summary_data <- filtered_data %>%
    group_by(elasmo) %>%
    summarise(
      total_detection = n(),
      mintime = min(time),
      maxtime = max(time),
      nbrstation = n_distinct(location),
      nbrday = n_distinct(date(time)),
      .groups = "drop"
    ) %>%
    mutate(
      difd = as.numeric(difftime(maxtime, mintime, units = "days")),
      difh = as.numeric(difftime(maxtime, mintime, units = "hours"))
    )

  ind_data %>%
    filter(acoustic_tag_id %in% summary_data$elasmo) %>%
    inner_join(summary_data, by = c("acoustic_tag_id" = "elasmo")) %>%
    mutate(
      at_liberty = pmax(1, as.numeric(difftime(maxtime, tagging_datetime, units = "days"))),
      residency_index = nbrday / at_liberty,
      across(c(mintime, maxtime, tagging_datetime), ~format(., "%Y-%m-%d %H:%M", tz = timezone))
    ) %>%
    select(
      ID = acoustic_tag_id, Species = species, `Date tagged` = tagging_datetime,
      Sex = sex, PCL = pcl, FL = fl, TL = tl, STL = stl, `Total detections` = total_detection,
      `Total Stations detected` = nbrstation, `Total days detected` = nbrday,
      `First appearance` = mintime, `Last appearance` = maxtime,
      `Days at liberty` = at_liberty, `Residency Index` = residency_index
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
}

#-------------------------------------------------------------------------------
# Function: generate_detection_matrix
#-------------------------------------------------------------------------------
#' @description Generates detection matrix (locations/agencies x individuals).
#' @param filtered_data Filtered detection data for specific time interval.
#' @param group_by_agency Group by agency instead of location (TRUE/FALSE).
#' @return Tibble with detection matrix.
generate_detection_matrix <- function(filtered_data, group_by_agency = FALSE) {
  if (nrow(filtered_data) == 0) return(tibble())

  group_var <- if (group_by_agency) sym("agency") else sym("location")

  filtered_data %>%
    count(!!group_var, elasmo) %>%
    pivot_wider(
      names_from = elasmo,
      values_from = n,
      values_fill = 0,
      names_sort = TRUE
    )
}

#-------------------------------------------------------------------------------
# Function: save_analysis_outputs
#-------------------------------------------------------------------------------
#' @description Saves analysis outputs to CSV files.
#' @param object Object to save.
#' @param save_path Directory path for saving.
#' @param file_name Base file name.
#' @param interval_id Time interval ID.
save_analysis_outputs <- function(object, save_path, file_name, interval_id = NULL) {
  full_path <- if (!is.null(interval_id)) {
    file.path(save_path, paste0(file_name, "_", interval_id, ".csv"))
  } else {
    file.path(save_path, paste0(file_name, ".csv"))
  }

  write_csv(object, full_path, na = "")
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Main Function: datasummary
#-------------------------------------------------------------------------------
#' @description Main function to generate detection summaries.
#' @param dir Base directory path.
#' @param saveloc Output directory path.
#' @param timezone Timezone for date handling.
#' @param start.p Optional start date (POSIXct).
#' @param timeint Use time intervals (TRUE/FALSE).
#' @param agency Group by agency (TRUE/FALSE).
datasummary <- function(
    dir = here::here("data"),
    saveloc = here::here("output"),
    timezone = "US/Eastern",
    start.p = NULL,
    timeint = FALSE,
    agency = FALSE
) {
  # Create output directory if needed
  if (!dir.exists(saveloc)) dir.create(saveloc, recursive = TRUE)

  # Load data
  det_cleaned <- readRDS(file.path(dir, "DET_cleaned.rds"))

  ind_data <- readRDS(file.path(dir, "IND.rds"))

  # Create time intervals
  time_data <- create_time_intervals(det_cleaned, start.p, timeint)
  timeseq <- time_data$timeseq

  # Save time interval metadata
  save_analysis_outputs(
    tibble(index = seq_along(timeseq), period = timeseq),
    saveloc,
    "dattime"
  )

  # Process each time interval
  walk(seq_len(length(timeseq) - 1), function(k) {
    interval_data <- det_cleaned %>%
      filter(time >= timeseq[k], time < timeseq[k + 1])

    # Generate and save individual summaries
    individual_summary <- generate_individual_summary(interval_data, ind_data, timezone)
    save_analysis_outputs(individual_summary, saveloc, "summary", k)

    # Generate and save detection matrices
    detection_matrix <- generate_detection_matrix(interval_data, agency)
    save_analysis_outputs(detection_matrix, saveloc, "matrix_ind_location", k)
  })

  message("Analysis complete. Outputs saved to: ", saveloc)
}

