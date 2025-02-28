#-------------------------------------------------------------------------------
# Script: Compare RDS Files (with enhanced diagnostics and standardization)
# Author: Sprinkles
# Date:   2025-02-28
#
# Description:
# This script compares two RDS files, checks if they are identical, and
# provides detailed diagnostics about any differences found. It includes
# enhanced standardization for row names and time columns.
#
# Dependencies:
# - here
# - dplyr
# - arsenal
# - purrr
# - lubridate
# - tibble
#
# Notes:
# - Assumes RDS files are stored in a 'data' directory.
# - Provides enhanced diagnostics about differences between datasets.
# - Standardizes row names and time columns for more accurate comparison.
#-------------------------------------------------------------------------------

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, arsenal, purrr, lubridate, tibble)

# Fresh Start
rm(list = ls())

#-------------------------------------------------------------------------------
# Function: standardize_time
#-------------------------------------------------------------------------------
#' @description Standardize time format more strictly
#' @param x A vector of time values
#' @return A vector of standardized POSIXct time values
standardize_time <- function(x) {
  if (is.POSIXct(x) || is.POSIXlt(x)) {
    return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  }
  return(x)
}

#-------------------------------------------------------------------------------
# Function: standardize_and_convert
#-------------------------------------------------------------------------------
#' @description Standardize NA values, convert types, standardize time, and reset row names
#' @param df A data frame or tibble
#' @return A standardized data frame with appropriate column types, NA values, time format, and reset row names
standardize_and_convert <- function(df) {
  df %>%
    as.data.frame() %>%  # Convert to standard dataframe
    remove_rownames() %>%  # Remove existing row names
    mutate(across(everything(), ~{
      if (is.character(.)) {
        ifelse(. %in% c("NA", "<NA>", ""), NA_character_, .)
      } else if (is.factor(.)) {
        fct <- factor(., exclude = NULL)  # Ensure NA is a level
        levels(fct)[levels(fct) %in% c("NA", "<NA>")] <- NA_character_
        fct
      } else {
        .
      }
    })) %>%
    mutate(across(where(is.character), ~{
      if(all(is.na(.) | grepl("^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*$", ., perl = TRUE))) {
        as.numeric(.)
      } else {
        .
      }
    })) %>%
    mutate(across(everything(), standardize_time)) %>%  # Standardize time format
    rownames_to_column("row_id")  # Add a standard row ID column
}

#-------------------------------------------------------------------------------
# Function: compare_rds_files_diagnostic
#-------------------------------------------------------------------------------
#' @description Compare Two RDS Files with Enhanced Diagnostics
#' @param new_file_name Name of the new RDS file
#' @param old_file_name Name of the old RDS file
#' @param data_dir Directory where the RDS files are stored (default: "data")
#' @return A list containing comparison results and detailed diagnostics
compare_rds_files_diagnostic <- function(new_file_name, old_file_name, data_dir = "data") {
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

  # Detailed diagnostics
  diagnostics <- list(
    column_names_match = identical(names(new_data), names(old_data)),
    column_types_match = all(sapply(1:ncol(new_data), function(i) identical(class(new_data[[i]]), class(old_data[[i]])))),
    row_counts_match = nrow(new_data) == nrow(old_data),
    column_counts_match = ncol(new_data) == ncol(old_data)
  )

  # Additional diagnostics
  diagnostics$attributes_equal <- all.equal(attributes(new_data), attributes(old_data))

  # Round numeric columns and compare
  new_data_rounded <- new_data %>% mutate(across(where(is.numeric), ~round(., 6)))
  old_data_rounded <- old_data %>% mutate(across(where(is.numeric), ~round(., 6)))
  diagnostics$identical_after_rounding <- identical(new_data_rounded, old_data_rounded)

  # Encode character columns and compare
  new_data_encoded <- new_data %>% mutate(across(where(is.character), ~iconv(., to = "UTF-8")))
  old_data_encoded <- old_data %>% mutate(across(where(is.character), ~iconv(., to = "UTF-8")))
  diagnostics$identical_after_encoding <- identical(new_data_encoded, old_data_encoded)

  # If still not identical, check for differences in individual columns
  if (!diagnostics$identical_after_encoding) {
    diagnostics$column_differences <- sapply(names(new_data), function(col) {
      if (!identical(new_data_encoded[[col]], old_data_encoded[[col]])) {
        diff_indices <- which(new_data_encoded[[col]] != old_data_encoded[[col]])
        if (length(diff_indices) > 0) {
          return(list(
            column = col,
            different_rows = diff_indices,
            sample_differences = head(data.frame(
              new_value = new_data_encoded[[col]][diff_indices],
              old_value = old_data_encoded[[col]][diff_indices]
            ), 5)
          ))
        }
      }
      return(NULL)
    })
    diagnostics$column_differences <- diagnostics$column_differences[!sapply(diagnostics$column_differences, is.null)]
  }

  # Return results
  list(
    are_identical = are_identical,
    comparison_summary = summary(comparison),
    diagnostics = diagnostics
  )
}

# Usage example
results <- compare_rds_files_diagnostic("DET_compiled.rds", "DET_compiled_old.rds")

# Print detailed results
cat("Are files identical?", results$are_identical, "\n\n")
print(results$comparison_summary)

cat("\nEnhanced Diagnostics:\n")
print(results$diagnostics)

if (!is.null(results$diagnostics$column_differences)) {
  cat("\nColumn-wise differences:\n")
  print(results$diagnostics$column_differences)
}
