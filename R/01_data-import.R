#-------------------------------------------------------------------------------
# Script: Generate Detection and Metadata Files
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-02-24
#
# Description:
# This script imports data from Google Drive, cleans and transforms it,
# and generates metadata files for acoustic telemetry data.
#
# Dependencies:
# - tidyverse
# - googledrive
# - readxl
#
# Notes:
# - Requires Google Drive authentication.
# - Assumes data is in a specific format (see documentation for details).
#-------------------------------------------------------------------------------

# Fresh Start # Un-comment below code if you want to run in this script
# rm(list = ls())

# Load Libraries # Un-comment below code if you want to run in this script
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(here, tidyverse, googledrive, readxl, janitor)

# Global Configuration # Un-comment below code if you want to run in this script
# config <- list(
#   data_timezone = "US/Eastern",
#   data_directory = here::here("data"),
#   excluded_locations = c(
#     "SOM1", "SOM2", "BBC1", "MB2", "MB3", "MB4", "MB5",
#     "BWC", "GCC", "Deep Drop 2", "Salvador",
#     "Deep Drop 1", "Bightbackreef", "On Buoy"
#   ),
#   otn_short_folder_url = "https://drive.google.com/drive/folders/1kShVtR3it9WUlVg9L4HzFcNA9R2_LYrN",
#   otn_short_file_pattern = "otn-instrument-deployment-short-form_GUTTRIDGE_2024_SEPT 24_1.xlsx",
#   catch_folder_url = "https://drive.google.com/drive/folders/1LzoZdCqBDhpYQEBc6gb-M2zBdFgKnQop",
#   catch_file_pattern = "SharkCapture.xlsx",
#   detection_folder_path = "https://drive.google.com/drive/folders/1oE72VHV4L_Gwm5zk48eEtOSBhMvzu99q"
# )

# Function Definitions

#-------------------------------------------------------------------------------
# Function: authenticate_drive
#-------------------------------------------------------------------------------
#' @description Authenticates the user with Google Drive.
#' This function uses the `googledrive` package to authenticate the user,
#' enabling access to Google Drive files and folders.
#' @return None. The function performs authentication as a side effect.
authenticate_drive <- function() {
  tryCatch(
    {
      drive_auth()
    },
    error = function(e) {
      stop("Failed to authenticate with Google Drive: ", e$message)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: import_data_from_drive
#-------------------------------------------------------------------------------
#' @description Imports data from a Google Drive file.
#' @param config_list List of configuration parameters.
#' @param folder_url URL of the Google Drive folder containing the file.
#' @param file_pattern Pattern to match the filename in the Google Drive folder.
#' @param sheet (Optional) Sheet number or name to read from the Excel file (default: 1).
#' @param col_types (Optional) Column types for reading the Excel file (default: NULL).
#' @return A data frame containing the imported data.
#' @examples
#' # Example usage:
#' # otn_short <- import_data_from_drive(
#' #   folder_url = "...",
#' #   file_pattern = "otn-instrument-deployment-short-form...",
#' #   sheet = 2,
#' #   col_types = c("INS_SERIAL_NO" = "text")
#' # )
import_data_from_gdrive <- function(config_list, folder_url, file_pattern, sheet = 1, col_types = NULL) {
  tryCatch(
    {
      # Get the folder and file
      folder <- drive_get(folder_url)
      file <- drive_ls(folder, pattern = file_pattern)

      # Check if the file exists
      if (nrow(file) == 0) {
        stop(paste("File not found in Google Drive folder:", folder_url, "with pattern:", file_pattern))
      }

      # Download the file to a temporary location
      temp_file <- tempfile(fileext = ".xlsx")
      drive_download(file = file$id, path = temp_file, overwrite = TRUE)

      # The below code chunk L65-77 is used to correctly assign data type to 'INS_SERIAL_NO'when function used to import the OTN short form
      # Get all column names from the Excel file
      all_col_names <- names(read_excel(temp_file, sheet = sheet, n_max = 0))

      # Create a col_types vector with "guess" for all columns
      all_col_types <- rep("guess", length(all_col_names))
      names(all_col_types) <- all_col_names # Assign names to the vector

      # Overwrite the specific column type if it exists in the sheet
      if ("INS_SERIAL_NO" %in% all_col_names) {
        all_col_types["INS_SERIAL_NO"] <- "numeric"
      }
      if ("event_ts" %in% all_col_names) { # this is needed because of the difference in which R and google drive save and read dates
        all_col_types["event_ts"] <- "numeric" # Read event_ts as numeric
      }

      # Read the Excel file
      # The suppressWarnings() is used to silence harmless warnings related to code chunk L65-77 above
      data <- suppressWarnings(read_excel(temp_file,
        sheet = sheet, col_types = all_col_types,
        na = "NA"
      ))

      # Convert event_ts to proper time format (related to L81-83)
      if ("event_ts" %in% names(data)) {
        data$event_ts <- format(as.POSIXct((data$event_ts - 2) * 86400, origin = "1900-01-01", tz = "UTC"), "%H:%M:%S")
      }

      # Clean up the temporary file
      unlink(temp_file)

      return(data)
    },
    error = function(e) {
      # Handle errors
      cat("Error importing data:", e$message, "\n")
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: extract_receiver_metadata
#-------------------------------------------------------------------------------
#' @description Extracts receiver location metadata from the OTN short form data.
#' @param config_list List of configuration parameters.
#' @param otn_data The OTN short form data frame.
#' @param excluded_locations (Optional) A vector of location names to exclude (Default: NULL).
#' @return A data frame containing receiver location metadata.
extract_receiver_metadata <- function(config_list, otn_data, excluded_locations = NULL) {
  rec_attr <- otn_data %>%
    filter(
      !(station_no == "SHARKHOLE" & deploy_lat == "24.42684") & # remove retired location
        ins_model_no == "VR2W" & # keep receivers only
        (is.null(excluded_locations) | !station_no %in% excluded_locations) # exclude locations, if specified
    ) %>%
    select( # select columns we want to keep
      deploy_lat,
      deploy_long,
      station_no,
      bottom_depth,
      riser_length,
      instrument_depth
    ) %>%
    rename( # rename for clarity
      location = station_no
    ) %>%
    mutate(
      deploy_lat = as.numeric(deploy_lat),
      deploy_long = as.numeric(deploy_long),
      agency = "STB (BAH)",
      location = str_replace_all(str_squish(str_to_lower(location)), "\\s", "_") # Converts strings to lowercase, trim leading/trailing whitespace
    ) %>%
    arrange(
      location
    ) %>%
    distinct( # remove duplicates if any
      deploy_lat,
      deploy_long,
      location,
      .keep_all = TRUE
    )

  return(rec_attr)
}

#-------------------------------------------------------------------------------
# Function: extract_receiver_deployment_data
#-------------------------------------------------------------------------------
#' @description Extracts receiver deployment data from the OTN short form data.
#' @param config_list List of configuration parameters.
#' @param otn_data The OTN short form data frame.
#' @param excluded_locations (Optional) A vector of location names to exclude (Default: NULL).
#' @param timezone The timezone to use for converting date/time values.
#' @return A data frame containing receiver deployment data.
extract_receiver_deployment_data <- function(config_list, otn_data, excluded_locations = NULL, timezone) {
  rec_mov <- otn_data %>%
    filter(
      !(station_no == "SHARKHOLE" & deploy_lat == "24.42684") & # retired station
        ins_model_no == "VR2W" &
        (is.null(excluded_locations) | !station_no %in% excluded_locations) & # exclude locations, if specified
        deploy_date_time_yyyy_mm_dd_thh_mm_ss != "2022-xxxxx" & # remove rows with incomplete metadata
        data_downloaded_y_n == "Y" & # only keep rows that show a complete receiver cycle (deploy + retrieve)
        !(station_no == "On Buoy" & deploy_date_time_yyyy_mm_dd_thh_mm_ss == "2023-06-23T15:00:00") # NOTE: TEMPORARY FILTER. Awaiting (meta)data for this location
    ) %>%
    select(
      ins_serial_no,
      deploy_date_time_yyyy_mm_dd_thh_mm_ss,
      recover_date_time_yyyy_mm_dd_thh_mm_ss,
      station_no,
      comments
    ) %>%
    mutate(
      ins_serial_no = str_remove(ins_serial_no, "\\.0$"), # remove ".0" from strings; these are unexplainably introduced when importing directly from google drive
      deploy_date_time_yyyy_mm_dd_thh_mm_ss = str_replace(deploy_date_time_yyyy_mm_dd_thh_mm_ss, "T", " "), # remove the "T" in the middle
      recover_date_time_yyyy_mm_dd_thh_mm_ss = str_replace(recover_date_time_yyyy_mm_dd_thh_mm_ss, "T", " "),
      deploy_date_time_yyyy_mm_dd_thh_mm_ss = as.POSIXct(deploy_date_time_yyyy_mm_dd_thh_mm_ss, format = "%Y-%m-%d %H:%M:%S", tz = timezone), # convert to posixct
      recover_date_time_yyyy_mm_dd_thh_mm_ss = as.POSIXct(recover_date_time_yyyy_mm_dd_thh_mm_ss, format = "%Y-%m-%d %H:%M:%S", tz = timezone),
      station_no = str_replace_all(str_squish(str_to_lower(station_no)), "\\s", "_") # Converts strings to lowercase, trim leading/trailing whitespace
    ) %>%
    rename(
      location = station_no,
      station = ins_serial_no,
      date_in = deploy_date_time_yyyy_mm_dd_thh_mm_ss,
      date_out = recover_date_time_yyyy_mm_dd_thh_mm_ss
    ) %>%
    arrange(
      location,
      date_in
    ) %>%
    distinct()

  return(rec_mov)
}

#-------------------------------------------------------------------------------
# Function: extract_tag_metadata
#-------------------------------------------------------------------------------
#' @description Extracts tag metadata from the catch data.
#' @param config_list List of configuration parameters.
#' @param catch_data The catch data frame.
#' @param timezone The timezone to use for converting date/time values.
#' @return A data frame containing tag metadata.
extract_tag_metadata <- function(config_list, catch_data, timezone) {
  ind_attr <- catch_data %>%
    filter(
      !(is.na(acoustic_tag_id)) & # filter out non-acoustic ids
        trimws(acoustic_tag_id) != "NA"
    ) %>%
    mutate(
      tagging_datetime = as.POSIXct(paste(event_dt, event_ts), format = "%Y-%m-%d %H:%M:%S", tz = timezone)
    ) %>%
    select(
      acoustic_tag_id,
      tagging_datetime,
      species,
      sex,
      pcl,
      fl,
      tl,
      stl,
      tag_owner,
      latitude,
      longitude,
      site
    ) %>%
    distinct(
      across(acoustic_tag_id),
      .keep_all = TRUE # removes duplicates which arise from re-entering acoustic ids when tagged shark gets recaptured
    ) %>%
    arrange(
      species,
      acoustic_tag_id
    )
  return(ind_attr)
}

#-------------------------------------------------------------------------------
# Function: process_detection_files
#-------------------------------------------------------------------------------
#' @description Processes acoustic detection files from a Google Drive folder.
#' @param config_list List of configuration parameters.
#' @param folder_path The path to the Google Drive folder.
#' @return A data frame containing the processed detection data.
process_detection_files <- function(config_list, folder_path) {
  # List all CSV files in the specified folder
  csv_files <- drive_ls(path = folder_path, pattern = "\\.csv$", recursive = TRUE)

  # Download and read all CSV files
  myfiles <- map(csv_files$id, function(file_id) {
    temp_file <- tempfile(fileext = ".csv")
    drive_download(file = as_id(file_id), path = temp_file, overwrite = TRUE)

    df <- suppressWarnings(read_csv(temp_file, # suppress false alarm warnings that 10 columns are expected (=10 column headers) but
      # in actuality there are only 8 (=data until 8th column)
      col_types = cols(.default = "c")
    )) # read all columns as character

    unlink(temp_file) # delete the temporary file

    return(df)
  })

  # Process each dataframe
  myfiles <- map(myfiles, function(df) {
    df %>%
      select(
        `Date and Time (UTC)`,
        Receiver,
        Transmitter,
        `Sensor Value`,
        `Sensor Unit`
      ) %>%
      rename(
        time = `Date and Time (UTC)`,
        station = Receiver,
        elasmo = Transmitter,
        sensor_value = `Sensor Value`,
        sensor_unit = `Sensor Unit`
      ) %>%
      mutate(
        time = str_trim(time),
        station = str_extract(station, "[^\\-]+$"),
        elasmo = str_extract(elasmo, "[^\\-]+$"),
        time = parse_date_time(
          time,
          orders = c(
            "Y-m-d H:M:S", "Y-m-d H:M",
            "d-m-y H:M:S", "d-m-y H:M",
            "y-m-d H:M:S", "y-m-d H:M",
            "Y/m/d H:M:S", "Y/m/d H:M",
            "d/m/y H:M:S", "d/m/y H:M",
            "d/m/Y H:M:S", "d/m/Y H:M",
            "d-m-Y H:M:S", "d-m-Y H:M"
          ),
          tz = "UTC",
          exact = FALSE
        ),
        elasmo = as.numeric(elasmo),
        agency = "STB (BAH)"
      ) %>%
      arrange(time)
  })

  # Combine all data frames
  raw_det <- bind_rows(myfiles)
  return(raw_det)
}

#-------------------------------------------------------------------------------
# Function: process_all_data
#-------------------------------------------------------------------------------
#' @description Processes all raw data, extracts metadata, and saves processed data.
#' This function orchestrates the entire data processing workflow, including:
#'  - Authenticating with Google Drive
#'  - Importing data from Google Drive (OTN short form and catch data)
#'  - Cleaning column names
#'  - Extracting receiver and tag metadata
#'  - Processing detection files
#'  - Saving the processed data as RDS files.
#' @param config_list List of configuration parameters.
#' @return None. The function saves processed data to RDS files as a side effect.
#' @examples
#' # Example usage:
#' # config <- list(
#' #   data_timezone = "US/Eastern",
#' #   data_directory = here::here("data"),
#' #   excluded_locations = c("SOM1", "SOM2"),
#' #   otn_short_folder_url = "...",
#' #   otn_short_file_pattern = "otn-instrument-deployment-short-form...",
#' #   catch_folder_url = "...",
#' #   catch_file_pattern = "SharkCapture.xlsx",
#' #   detection_folder_path = "..."
#' # )
#' # process_all_data(config)
process_all_data <- function(config_list) {
  # Authenticate with Google Drive
  authenticate_drive()

  tryCatch(
    {
      # Import Data
      otn_short <- import_data_from_gdrive(
        config_list,
        folder_url = config_list$otn_short_folder_url,
        file_pattern = config_list$otn_short_file_pattern,
        sheet = 2
      )

      catch <- import_data_from_gdrive(
        config_list,
        folder_url = config_list$catch_folder_url,
        file_pattern = config_list$catch_file_pattern,
        sheet = 1
      )

      # 2. Clean Column Names
      otn_short <- janitor::clean_names(otn_short)

      # 3. Extract Metadata
      rec_attr <- extract_receiver_metadata(
        config_list,
        otn_data = otn_short,
        excluded_locations = config_list$excluded_locations
      )

      rec_mov <- extract_receiver_deployment_data(
        config_list,
        otn_data = otn_short,
        excluded_locations = config_list$excluded_locations,
        timezone = config_list$data_timezone
      )

      ind_attr <- extract_tag_metadata(
        config_list,
        catch_data = catch,
        timezone = config_list$data_timezone
      )

      # 4. Process Detection Files
      raw_det <- process_detection_files(
        config_list,
        folder_path = config_list$detection_folder_path
      )

      # 5. Save Processed Data
      data_files <- list(
        vloc = rec_attr,
        vmov = rec_mov,
        ind = ind_attr,
        det = raw_det
      )

      purrr::iwalk(data_files, ~ saveRDS(.x, file.path(config_list$data_directory, paste0(.y, ".rds"))))
      print("All data process complete")
    },
    error = function(e) {
      message("Error during execution: ", e$message)
    }
  )
}
