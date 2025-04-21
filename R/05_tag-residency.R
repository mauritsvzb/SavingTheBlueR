#-------------------------------------------------------------------------------
# Script: Calculate Temporal Residency of Acoustic Tags
# Author: Maurits van Zinnicq Bergmann
# Date Created: 2025-04-20
#
# Description:
# Calculates proportional tag activity (detections/tags-at-liberty) per species
# aggregated monthly across years. Visualizes temporal residency patterns.
#
# Methodology:
# 1. Calculates active tags from detection data
# 2. Computes cumulative tags-at-liberty from deployment records
# 3. Joins data sets using rolling date alignment
# 4. Calculates monthly proportions across years
# 5. Generates standardized temporal residency plot
#
# Dependencies:
# - tidyverse
# - data.table
#-------------------------------------------------------------------------------

# Fresh Start
# rm(list = ls())

# Load Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, data.table, here)

# Global Configuration
# config_list <- list(
#   detections_file = "det_cleaned_season.rds",
#   individual_attribute_file = "ind.rds",
#   data_directory = here::here("data"), # Main project data directory
#   output_directory = here::here("output/seasonality"), # Directory to save plots
#   output_plot = "temporal_residency.png", # Name of output plot
#   plot_width = 8,
#   plot_height = 6,
#   plot_dpi = 300,
#   font_family = "Times", # Font family
#   text_size = 16 # UText size
# )

# Function Definitions

#-------------------------------------------------------------------------------
# Function: calculate_active_tags
#-------------------------------------------------------------------------------
#' @description Calculate monthly active tags per species
#' @param det Data frame containing detection data
#' @param ind Data frame containing individual tag metadata
#' @return Data frame with active tag counts
calculate_active_tags <- function(det, ind) {
  det %>%
    inner_join(
      ind %>%
        select(acoustic_tag_id, species),
      by = c("elasmo" = "acoustic_tag_id")
    ) %>%
    mutate(
      ym = format(as.Date(time), "%Y-%m")
    ) %>%
    group_by(species, ym) %>%
    summarise(active_count = n_distinct(elasmo), .groups = "drop")
}

#-------------------------------------------------------------------------------
# Function: calculate_deployed_tags
#-------------------------------------------------------------------------------
#' @description Calculate cumulative tags deployed per species
#' @param ind Data frame containing individual tag metadata
#' @return Data frame with cumulative deployment counts
calculate_deployed_tags <- function(ind) {
  ind %>%
    semi_join(
      det,
      by = c("acoustic_tag_id" = "elasmo")
    ) %>%
    mutate(date_tagged = format(as.Date(tagging_datetime), "%Y-%m")) %>%
    select(species, acoustic_tag_id, date_tagged) %>%
    group_by(species, date_tagged) %>%
    summarise(deploy_count = n_distinct(acoustic_tag_id), .groups = "drop") %>%
    mutate(cs = cumsum(deploy_count)) %>%
    select(-deploy_count)
}

#-------------------------------------------------------------------------------
# Function: join_activity_deployment
#-------------------------------------------------------------------------------
#' @description Join active and deployed tags using rolling date alignment
#' @param active Data frame from calculate_active_tags
#' @param deployed Data frame from calculate_deployed_tags
#' @return Joined data frame with proportional calculations
join_activity_deployment <- function(active, deployed) {
  # Convert to date objects
  active_dt <- data.table(
    active %>%
      # This step makes sure this column can be transformed to a 'date' column
      # but we don't care about the day
      mutate(ym = as.Date(paste0(ym, "-01"))),
    key = c("species", "ym")
  )

  deployed_dt <- data.table(
    deployed %>%
      # This step makes sure this column can be transformed to a 'date' column
      # but we don't care about the day
      mutate(date_tagged = as.Date(paste0(date_tagged, "-01"))),
    key = c("species", "date_tagged")
  )

  # Create a designated RollDate column for rolling join
  active_dt[, RollDate := ym]
  deployed_dt[, RollDate := date_tagged]

  # Perform the rolling join
  agg <- deployed_dt[
    active_dt,
    roll = TRUE,
    on = .(species, RollDate)
  ]

  # Clean up results
  agg %>%
    select(-date_tagged,
           -ym)
}

#-------------------------------------------------------------------------------
# Function: create_skeleton_grid
#-------------------------------------------------------------------------------
#' @description Create complete date grid for all species
#' @param joined_data Data frame from join_activity_deployment
#' @param original_detections Full detection data for date ranges
#' @return Data frame with complete date-species combinations
create_skeleton_grid <- function(joined_data, original_detections) {
  setDT(original_detections)[, .(
    RollDate = seq(
      from = min(joined_data$RollDate, na.rm = TRUE),
      to = max(joined_data$RollDate, na.rm = TRUE),
      by = "1 month"
    )
  ), by = species]
}

#-------------------------------------------------------------------------------
# Function: calculate_monthly_proportions
#-------------------------------------------------------------------------------
#' @description Calculate final monthly proportions across years
#' @param joined_data Data frame from join_activity_deployment
#' @param skeleton_grid Data frame from create_skeleton_grid
#' @return Data frame with final proportional calculations
calculate_monthly_proportions <- function(joined_data, skeleton_grid) {
  agg <- skeleton_grid %>%
    left_join(joined_data, by = c("species", "RollDate")) %>%
    replace(is.na(.), 0) %>% # Replace NA's with 0
    mutate(
      active_prop = round(active_count / cs, 1),
      M = format(RollDate, "%m")
    )

  agg %>%
    group_by(species, M) %>%
    summarise(mean_active_prop = mean(active_prop), .groups = "drop") %>%
    mutate(species = as.character(species)) %>%
    arrange(species)
}

#-------------------------------------------------------------------------------
# Function: create_residency_plot
#-------------------------------------------------------------------------------
#' @description Generate standardized residency plot
#' @param proportions Data frame from calculate_monthly_proportions
#' @return ggplot object
create_residency_plot <- function(proportions, font_family, text_size) {
  ggplot(proportions, aes(x = M, y = mean_active_prop, group = species,
                          color = species)) +
    geom_line(aes(linetype = species)) +
    labs(x = 'Month of year', y = 'Proportional tags active', color = "Species", linetype = "Species") +
    theme_bw() +
    theme(
      text = element_text(size = text_size, family = font_family),
      legend.title = element_text(size = text_size, family = font_family), # Size of legend title
      legend.text = element_text(face = "italic") # Italicize legend texxt
    ) +
  ylim(0, 1)
}

#-------------------------------------------------------------------------------
# Function: process_residency_analysis
#-------------------------------------------------------------------------------
#' @description Execute full analysis pipeline
#' @param config_list Configuration list
#' @return Invisibly returns TRUE if successful
#' @description Complete residency analysis pipeline
#' @param config_list List of configuration parameters
#' @return Invisibly returns TRUE if successful
process_residency_analysis <- function(config_list) {
  # Load data
  det <- readRDS(file.path(config_list$data_directory, config_list$detections_file))
  ind <- readRDS(file.path(config_list$data_directory, config_list$individual_attribute_file))

 # Processing pipeline
  active <- calculate_active_tags(det, ind)
  deployed <- calculate_deployed_tags(ind)
  joined <- join_activity_deployment(active, deployed)

  # Add 'species' back into 'df' so that create_skeleton_grid() can use it
  det <- det %>%
    inner_join(
      ind %>%
        select(acoustic_tag_id, species),
      by = c("elasmo" = "acoustic_tag_id")
    )

  skeleton <- create_skeleton_grid(joined, det)

  proportions <- calculate_monthly_proportions(joined, skeleton)

  # Generate plot
  p <- create_residency_plot(proportions, font_family = config_list$font_family, text_size = config_list$text_size)

  ggsave(filename = file.path(config_list$output_directory, config_list$output_plot),
         plot = p,
         width = config_list$plot_width,
         height = config_list$plot_height,
         dpi = config_list$plot_dpi
         )

}
