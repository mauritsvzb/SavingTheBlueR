#-------------------------------------------------------------------------------
# Script: Compare RDS Files
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-02-28
#
# Description:
# This script compares two RDS files, checks if they are identical, and
# identifies unique rows in both directions. It handles different representations
# of NA values, converts character columns to numeric where possible, ensures
# consistent data structure between tibbles and dataframes, and standardizes
# time formats.
#
# Dependencies:
# - here
# - dplyr
# - arsenal
# - purrr
# - lubridate
#
# Notes:
# - Assumes RDS files are stored in a 'data' directory.
# - Returns a list containing comparison results and unique rows in both directions.
# - Automatically converts character columns to numeric where possible.
# - Standardizes NA values and time formats across datasets before comparison.
# - Converts tibbles to dataframes for consistent comparison.
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, arsenal, purrr, lubridate)

#-------------------------------------------------------------------------------
# Function: standardize_time
#-------------------------------------------------------------------------------
#' @description Standardize time format
#' @param x A vector of time values
#' @return A vector of standardized POSIXct time values
standardize_time <- function(x) {
  if (is.POSIXct(x) || is.POSIXlt(x)) {
    return(force_tz(x, tzone = "US/Eastern")) ####### MO NOTE: make this is user defined parameter
  }
  return(x)
}

#-------------------------------------------------------------------------------
# Function: standardize_and_convert
#-------------------------------------------------------------------------------
#' @description Standardize NA values, convert types, and standardize time in a dataframe
#' @param df A data frame or tibble
#' @return A standardized data frame with appropriate column types, NA values, and time format
standardize_and_convert <- function(df) {
  df %>%
    as.data.frame() %>% # Convert to standard dataframe
    mutate(across(everything(), ~ {
      if (is.character(.)) {
        ifelse(. %in% c("NA", "<NA>", ""), NA_character_, .)
      } else if (is.factor(.)) {
        fct <- factor(., exclude = NULL) # Ensure NA is a level
        levels(fct)[levels(fct) %in% c("NA", "<NA>")] <- NA_character_
        fct
      } else {
        .
      }
    })) %>%
    mutate(across(where(is.character), ~ {
      if (all(is.na(.) | grepl("^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*$", ., perl = TRUE))) {
        as.numeric(.)
      } else {
        .
      }
    })) %>%
    mutate(across(everything(), standardize_time)) # Standardize time format
}

#-------------------------------------------------------------------------------
# Function: compare_rds_files
#-------------------------------------------------------------------------------
#' @description Reads two RDS files, compares them, and returns the differences.
#' @param new_file_name Name of the new RDS file
#' @param old_file_name Name of the old RDS file
#' @param data_dir Directory where the RDS files are stored (default: "data")
#' @return A list containing comparison results and unique rows in both directions
#' @examples
#' compare_rds_files("DET_compiled.rds", "DET_compiled_old.rds")
compare_rds_files <- function(new_file_name, old_file_name, data_dir = "data") {
  # Read RDS files
  new_data <- tryCatch(
    readRDS(here(data_dir, new_file_name)),
    error = function(e) stop("Error reading new file: ", e$message)
  )

  old_data <- tryCatch(
    readRDS(here(data_dir, old_file_name)),
    error = function(e) stop("Error reading old file: ", e$message)
  )

  # Standardize NA values, convert types, and ensure dataframe structure
  new_data <- standardize_and_convert(new_data)
  old_data <- standardize_and_convert(old_data)

  # Check if files are identical
  are_identical <- identical(new_data, old_data)

  # Compare dataframes
  comparison <- comparedf(new_data, old_data)

  # Find unique rows in new_data not in old_data
  unique_rows_new <- anti_join(new_data, old_data)

  # Find unique rows in old_data not in new_data
  unique_rows_old <- anti_join(old_data, new_data)

  # Return results
  list(
    are_identical = are_identical,
    comparison_summary = summary(comparison),
    unique_rows_new = unique_rows_new,
    unique_rows_old = unique_rows_old
  )
}

# Usage example
results <- compare_rds_files("DET_compiled.rds", "DET_compiled_old.rds")

# Print results
cat("Are files identical?", results$are_identical, "\n\n")
print(results$comparison_summary)

cat("\nUnique rows in new file (not in old file):\n")
print(results$unique_rows_new)

cat("\nUnique rows in old file (not in new file):\n")
print(results$unique_rows_old)

# Print summary of unique rows
cat("\nSummary of unique rows:\n")
cat("Number of rows unique to new file:", nrow(results$unique_rows_new), "\n")
cat("Number of rows unique to old file:", nrow(results$unique_rows_old), "\n")
