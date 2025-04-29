#-------------------------------------------------------------------------------
# Script: Convert Mixed Date Formats to POSIXct (Multi-Format Handling)
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-04-28
#
# Description:
# Converts character dates with mixed formats (m/d/yy and m/d/yyyy) to POSIXct
# with automatic format detection. Specifically handles acoustic telemetry data
# where date formats might change mid-column. Features format validation and
# detailed conversion metrics.
#
# Dependencies:
# - tidyverse (for data manipulation)
# - lubridate (for date parsing)
# - purrr (for functional programming)
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, purrr, here)

# Global Configuration
config_list <- list(
  input_file = "UF_2024_A69-9001-46839.csv", # .csv or .rds
  output_file = "UF_2024_A69-9001-46839_date-cleaned.csv", # .rds or .csv
  date_column = "Date and Time (UTC)",
  data_directory = here::here(),
  output_format = "csv", # "rds" or "csv"
  date_formats = c("%m/%d/%y %H:%M", "%m/%d/%Y %H:%M") # Add formats as needed
)

#-------------------------------------------------------------------------------
# Function: validate_date_data
#-------------------------------------------------------------------------------
#' @description Validate date column structure
#' @param .data A tibble/data frame
#' @param date_col Name of date column
#' @return Invisible TRUE if valid
#' @throws Error if validation fails
validate_date_data <- function(.data, date_col = "Date and Time (UTC)") {
  if (!date_col %in% names(.data)) {
    stop("Date column '", date_col, "' not found", call. = FALSE)
  }

  if (!is.character(.data[[date_col]])) {
    stop("Date column must be character type", call. = FALSE)
  }

  invisible(TRUE)
}

#-------------------------------------------------------------------------------
# Function: convert_mixed_dates
#-------------------------------------------------------------------------------
#' @description Convert mixed date formats with smart detection
#' @param date_vec Character vector of dates
#' @param format_list List of potential date formats
#' @param tz Timezone
#' @return POSIXct vector
convert_mixed_dates <- function(date_vec,
                                format_list = config_list$date_formats,
                                tz = "UTC") {

  # Smart parsing with fallback mechanism
  parse_datetime_safely <- function(x) {
    # Try explicit formats first
    for (fmt in format_list) {
      parsed <- parse_date_time(x, orders = fmt, exact = TRUE, quiet = TRUE)
      if (!any(is.na(parsed))) return(parsed)
    }
    # Fallback to flexible parsing
    parse_date_time(x, orders = c("mdy HM", "mdy HMS"), quiet = TRUE)
  }

  parsed_dates <- map_dbl(date_vec, ~ as.numeric(parse_datetime_safely(.x))) %>%
    as.POSIXct(origin = "1970-01-01", tz = tz)

  if (any(is.na(parsed_dates))) {
    warning("Some dates failed to parse completely", call. = FALSE)
  }

  parsed_dates
}

#-------------------------------------------------------------------------------
# Function: convert_dataframe_dates
#-------------------------------------------------------------------------------
#' @description Process date column in data frame
#' @param .data A tibble/data frame
#' @param date_col Name of date column
#' @param ... Additional params for convert_mixed_dates()
#' @return Modified tibble
convert_dataframe_dates <- function(.data,
                                    date_col = "Date and Time (UTC)",
                                    ...) {

  validate_date_data(.data, date_col)

  .data %>%
    mutate(!!sym(date_col) := convert_mixed_dates(!!sym(date_col), ...))
}

#-------------------------------------------------------------------------------
# Function: calculate_conversion_stats
#-------------------------------------------------------------------------------
#' @description Generate conversion metrics
#' @param .data Processed tibble
#' @param date_col Date column name
#' @return Invisible list with stats
calculate_conversion_stats <- function(.data, date_col) {
  stats <- list(
    total = nrow(.data),
    failed = sum(is.na(.data[[date_col]])),
    success_rate = mean(!is.na(.data[[date_col]])) * 100
  )

  str_glue(
    "Date Conversion Results\n",
    "-----------------------\n",
    "Total: {stats$total}\n",
    "Failed: {stats$failed}\n",
    "Success Rate: {round(stats$success_rate, 2)}%\n"
  ) %>% cat()

  invisible(stats)
}

#-------------------------------------------------------------------------------
# Function: load_detection_data
#-------------------------------------------------------------------------------
#' @description Load data from file
#' @param file_path Path to input file
#' @return Tibble
load_detection_data <- function(file_path) {
  ext <- tools::file_ext(file_path) %>% tolower()

  switch(
    ext,
    "rds" = read_rds(file_path),
    "csv" = read_csv(file_path, show_col_types = FALSE),
    stop("Unsupported file format: ", ext, call. = FALSE)
  )
}

#-------------------------------------------------------------------------------
# Function: save_detection_data
#-------------------------------------------------------------------------------
#' @description Save data to file
#' @param .data Tibble to save
#' @param file_path Output path
#' @param format Output format ("rds" or "csv")
#' @return Invisible TRUE
save_detection_data <- function(.data, file_path, format = c("auto", "rds", "csv")) {
  format <- match.arg(format)

  if (format == "auto") {
    format <- tools::file_ext(file_path) %>% tolower()
  }

  if (format == "csv") {
    # Use !!sym() to unquote the column name from config_list$date_column
    .data <- .data %>%
      mutate(!!sym(config_list$date_column) := format(!!sym(config_list$date_column), "%Y-%m-%d %H:%M"))
  }

  switch(
    format,
    "rds" = write_rds(.data, file_path),
    "csv" = write_csv(.data, file_path),
    stop("Unsupported output format: ", format, call. = FALSE)
  )

  invisible(TRUE)
}


#-------------------------------------------------------------------------------
# Function: process_all_dates
#-------------------------------------------------------------------------------
#' @description Main processing pipeline
#' @param config_list Configuration list
#' @return Invisible TRUE if successful
process_all_dates <- function(config_list) {
  # Set defaults
  config_list$output_format <- config_list$output_format %||% "rds"

  # Validate configuration
  if (!config_list$output_format %in% c("rds", "csv")) {
    stop("Output format must be either 'rds' or 'csv'", call. = FALSE)
  }

  # Prepare paths
  input_path <- here::here(config_list$data_directory, config_list$input_file)
  output_path <- here::here(
    config_list$data_directory,
    str_glue(
      "{tools::file_path_sans_ext(config_list$output_file)}.",
      "{config_list$output_format}"
    )
  )

  # Processing pipeline
  tryCatch(
    {
      # Load and convert
      result <- input_path %>%
        load_detection_data() %>%
        convert_dataframe_dates(date_col = config_list$date_column)

      # Generate stats
      calculate_conversion_stats(result, config_list$date_column)

      # Save results
      save_detection_data(
        result,
        output_path,
        format = config_list$output_format
      )

      str_glue("\nSuccessfully saved to {output_path}\n") %>% cat()
      invisible(TRUE)
    },
    error = function(e) {
      str_glue("\nERROR: {e$message}\n") %>% cat()
      invisible(FALSE)
    }
  )
}

#-------------------------------------------------------------------------------
# Example Usage
#-------------------------------------------------------------------------------
# config <- list(
#   input_file = "raw_detections.csv",
#   output_file = "detections_converted",
#   date_column = "Date and Time (UTC)",
#   data_directory = "data",
#   output_format = "csv"
# )
#
# process_all_dates(config_list)
