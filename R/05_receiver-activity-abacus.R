#-------------------------------------------------------------------------------
# Script: Receiver Activity Analysis
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-21
#
# Description:
# Analyzes receiver activity based on deployment and detection data,
# generating comparative abacus plots for visualization.
#
# Dependencies:
# - tidyverse, tidylog
#-------------------------------------------------------------------------------

# Fresh Start
# rm(list = ls())

# Load Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, tidylog)

# Global Configuration
# config <- list(
#   dirs = list(
#     root = here::here(), #NOT SURE THHIS IS REALLY NEEDED!?
#     data = here::here("data"),
#     output = here::here("output/silkies_2025")
#   ),
#   files = list(
#     det = "det_cleaned.rds", # Acoustic detections
#     vmov = "vmov.rds", # Receiver movements
#     ind = "ind.rds", # Individual attributes
#     vloc = "vloc.rds" # Receiver locations
#   ),
#   params = list(
#     time_zone = "US/Eastern",
#     plot_width = 15,
#     plot_height = 8
#   )
# )

# Function Definitions

#-------------------------------------------------------------------------------
# Function: load_and_validate_data
#-------------------------------------------------------------------------------
#' @description Load and validate core datasets
#' @param config_list List of configuration parameters
#' @param data_folder Name of folder that contains the input data
#' @return List of loaded datasets
#' @throws Error if required files are missing or invalid
load_and_validate_data <- function(config_list, data_folder) {
  required_files <- c("det", "vmov", "ind", "vloc")
  for (file in required_files) {
    file_path <- file.path(data_folder, config_list$files[[file]])
    if (!file.exists(file_path)) {
      stop(paste("Required file not found:", file_path))
    }
  }

  tryCatch(
    {
      list(
        det = readRDS(file.path(data_folder, config_list$files$det)),
        vmov = readRDS(file.path(data_folder, config_list$files$vmov)),
        ind = readRDS(file.path(data_folder, config_list$files$ind)),
        loc = readRDS(file.path(data_folder, config_list$files$vloc))
      )
    },
    error = function(e) {
      stop("Data loading failed: ", e$message)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: clean_receiver_data
#-------------------------------------------------------------------------------
#' @description Clean and preprocess receiver deployment data
#' @param config_list List of configuration parameters
#' @param vmov Raw vmov data
#' @param timezone Timezone for the data
#' @return Cleaned vmov data
clean_receiver_data <- function(config_list, vmov, timezone) {
  vmov %>%
    arrange(location, date_in) %>%
    filter( # Remove rows include failed receiver deployments
      !(date_in == as.POSIXct("2023-05-30 16:18:00", tz = timezone) & location == "bwcdrop"),
      !(date_in == as.POSIXct("2022-06-29 19:52:00", tz = timezone) & location == "cc1"),
      !(date_in == as.POSIXct("2023-05-31 17:21:00", tz = timezone) & location == "drop6"),
      !(date_in == as.POSIXct("2023-06-08 19:55:00", tz = timezone) & location == "mb1"),
    ) %>%
    mutate(
      date_out = case_when( # Change retrieval dates for ccdrop and drop 5 so that it reflects time date when the data error occurred?
        (date_in == as.POSIXct("2023-06-03 17:15:00", tz = timezone) &
          location == "drop5") ~ as.POSIXct("2023-07-10 12:42:00", tz = timezone),
        (date_in == as.POSIXct("2023-05-30 15:32:00", tz = timezone) &
          location == "ccdrop") ~ as.POSIXct("2023-07-23 12:42:00", tz = timezone),
        TRUE ~ date_out
      )
    )
}

#-------------------------------------------------------------------------------
# Function: create_weekly_deployment_data
#-------------------------------------------------------------------------------
#' @description Create weekly deployment activity data
#' @param config_list List of configuration parameters
#' @param vmov Cleaned vmov data
#' @param timezone Timezone for the data
#' @return Tibble with weekly deployment data
create_weekly_deployment_data <- function(config_list, vmov, timezone) {
  vmov %>%
    mutate(across(c(date_in, date_out), ~ as.Date(., tz = timezone))) %>%
    rowwise() %>%
    mutate(weeks = list(seq(date_in, date_out, by = "week"))) %>%
    unnest(weeks) %>%
    add_column(source = "Rec. mov.")
}

#-------------------------------------------------------------------------------
# Function: create_weekly_detection_data
#-------------------------------------------------------------------------------
#' @description Create weekly detection activity data
#' @param config_list List of configuration parameters
#' @param det Detection data
#' @param vmov Cleaned VMOV data
#' @param timezone Timezone for the data
#' @return Tibble with weekly detection data
create_weekly_detection_data <- function(config_list, det, vmov, timezone) {
  det %>%
    filter(agency == "STB (BAH)") %>%
    mutate(
      date = as.Date(time, tz = timezone)
    ) %>%
    inner_join(
      vmov,
      by = c("location", "station"),
      relationship = "many-to-many"
    ) %>%
    filter(date >= as.Date(date_in, tz = timezone) &
      date <= as.Date(date_out, tz = timezone)) %>%
    group_by(location, station, date_in, date_out) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      .groups = "drop"
    ) %>%
    group_by(location) %>%
    mutate(min_start_date = min(start_date)) %>%
    ungroup() %>%
    arrange(location, min_start_date, station) %>%
    rowwise() %>%
    mutate(weeks = list(seq(start_date, end_date, by = "week"))) %>%
    unnest(weeks) %>%
    mutate(source = "Det.")
}

#-------------------------------------------------------------------------------
# Function: add_offset
#-------------------------------------------------------------------------------
#' @description Add vertical offset to detection data points
#' @param data Combined dataset
#' @param offset Vertical offset amount (default: 0.4)
#' @return Dataset with y_position column
add_offset <- function(data, offset = 0.4) {
  if (!"source" %in% names(data)) {
    stop("Input data must contain 'source' column")
  }

  data %>%
    mutate(y_position = as.numeric(factor(location)) +
      ifelse(source == "Det.", offset, 0))
}

#-------------------------------------------------------------------------------
# Function: create_comparative_abacus
#-------------------------------------------------------------------------------
#' @description Create comparative abacus plot
#' @param vmov_data Weekly deployment data
#' @param det_data Weekly detection data
#' @return ggplot object
create_comparative_abacus <- function(vmov_data, det_data) {
  # Combine datasets with source identification
  combined_data <- bind_rows(
    vmov_data %>% select(location, weeks, source),
    det_data %>% select(location, weeks, source)
  )

  # Add offset for clear visualization of overlapping points
  combined_data <- combined_data %>%
    add_offset()

  ggplot(combined_data, aes(x = weeks, y = y_position, color = source)) +
    geom_point(shape = 16, size = 3, alpha = 0.8) +
    scale_x_date(
      date_breaks = "4 weeks",
      date_labels = "%Y-%m-%d",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      breaks = seq(1, length(unique(combined_data$location)), 1) + 0.2,
      labels = unique(combined_data$location),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_color_manual(
      values = c("Rec. mov." = "black", "Det." = "goldenrod2"),
      labels = c("Detection Data", "Receiver Deployment Records")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    ) +
    labs(
      x = "Date",
      y = "Receiver Location",
      title = "Receiver Activity Comparison",
      color = "Data Source"
    )
}

#-------------------------------------------------------------------------------
# Main Function: run_receiver_analysis
#-------------------------------------------------------------------------------
run_receiver_analysis <- function(config_list) {
  # Load and validate data
  data <- load_and_validate_data(config_list, data_folder = config_list$dirs$data)

  # Clean receiver deployment data
  vmov_clean <- clean_receiver_data(config_list, data$vmov, timezone = config_list$params$time_zone)

  # Generate analysis datasets
  weekly_vmov <- create_weekly_deployment_data(config_list, vmov_clean, timezone = config_list$params$time_zone)
  weekly_det <- create_weekly_detection_data(config_list, data$det, vmov_clean, timezone = config_list$params$time_zone)

  # Create comparative plot
  plot <- create_comparative_abacus(weekly_vmov, weekly_det)

  # Save plot
  ggsave(
    file.path(config_list$dirs$output, "receiver_activity_comparison.png"),
    plot = plot,
    width = config_list$params$plot_width,
    height = config_list$params$plot_height,
    dpi = 300
  )

  invisible(list(
    vmov_data = weekly_vmov,
    det_data = weekly_det,
    plot = plot
  ))

  print("Analysis complete. Plot saved in output directory.")
}
