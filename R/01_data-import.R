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

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, googledrive, readxl)

# Define Global Variables/Constants
data_timezone <- "US/Eastern"
data_directory <- here("data")

# Function Definitions

#-------------------------------------------------------------------------------
# Function: import_data_from_drive
#-------------------------------------------------------------------------------
#' @description Imports data from a Google Drive file.
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
import_data_from_gdrive <- function(folder_url, file_pattern, sheet = 1, col_types = NULL) {
  tryCatch({
    # Authenticate with Google Drive
    drive_auth()

    # Get the folder and file
    folder <- drive_get(folder_url)
    file <- drive_ls(folder, pattern = file_pattern)

    # Check if the file exists
    if (nrow(file) == 0) {
      stop(paste("File not found in Google Drive folder:", folder_url, "with pattern:", file_pattern))
    }

    # Download the file to a temporary location
    tempfile <- tempfile(fileext = ".xlsx")
    drive_download(file = file$id, path = tempfile, overwrite = TRUE)

    # Read the Excel file
    data <- read_excel(tempfile, sheet = sheet, col_types = col_types)

    # Clean up the temporary file
    unlink(tempfile)

    return(data)
  }, error = function(e) {
    # Handle errors
    cat("Error importing data:", e$message, "\n")
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: clean_column_names
#-------------------------------------------------------------------------------
#' @description Cleans column names by replacing spaces with dots,
#'              removing parentheses, and removing dollar signs.
#' @param data A data frame.
#' @return A data frame with cleaned column names.
clean_column_names <- function(data) {
  names(data) <- names(data) %>%
    gsub(" ", ".", .) %>%
    gsub("\\(", "", .) %>%
    gsub("\\)", "", .)
  return(data)
}

#-------------------------------------------------------------------------------
# Function: extract_receiver_metadata
#-------------------------------------------------------------------------------
#' @description Extracts receiver location metadata from the OTN short form data.
#' @param otn_data The OTN short form data frame.
#' @param excluded_stations A vector of station names to exclude.
#' @return A data frame containing receiver location metadata.
extract_receiver_metadata <- function(otn_data, excluded_stations) {
  rec.attr <- otn_data %>%
    filter(
      !(STATION_NO == "SHARKHOLE" & DEPLOY_LAT == "24.42684"), #remove retired location
      INS_MODEL_NO == "VR2W", #remove non-receivers
      !STATION_NO %in% excluded_stations #remove certain receiver locations
    ) %>%
    select( #select columns we want to keep
      DEPLOY_LAT,
      DEPLOY_LONG,
      STATION_NO,
      BOTTOM_DEPTH,
      RISER_LENGTH,
      INSTRUMENT_DEPTH
    ) %>%
    rename( #rename for convenience
      GPS.N = DEPLOY_LAT,
      GPS.W = DEPLOY_LONG,
      location = STATION_NO
    ) %>%
    mutate(
      GPS.N = as.numeric(GPS.N),
      GPS.W = as.numeric(GPS.W),
      agency = "STB (BAH)"
    ) %>%
    arrange(
      location
    ) %>%
    distinct( #remove duplicates
      GPS.N,
      GPS.W,
      location,
      .keep_all = TRUE
    )
  return(rec.attr)
}

#-------------------------------------------------------------------------------
# Function: extract_receiver_deployment_data
#-------------------------------------------------------------------------------
#' @description Extracts receiver deployment data from the OTN short form data.
#' @param otn_data The OTN short form data frame.
#' @param excluded_stations A vector of station names to exclude.
#' @param timezone The timezone to use for converting date/time values.
#' @return A data frame containing receiver deployment data.
extract_receiver_deployment_data <- function(otn_data, excluded_stations, timezone) {
  rec.mov <- otn_data %>%
    filter(
      !(STATION_NO == "SHARKHOLE" & DEPLOY_LAT == "24.42684"), #retired station
      INS_MODEL_NO == "VR2W",
      !STATION_NO %in% excluded_stations,
      `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss` != "2022-xxxxx", #remove rows with incomplete metadata
      `DATA_DOWNLOADED.y/n` == "Y", #only keep rows that show a complete receiver cycle (deploy + retrieve)
      !(STATION_NO == "On Buoy" & `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss` == "2023-06-23T15:00:00") #NOTE: TEMPORARY FILTER. Awaiting (meta)data for this location
    ) %>%
    select(
      INS_SERIAL_NO,
      `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss`,
      `RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss`,
      STATION_NO,
      COMMENTS
    ) %>%
    mutate(
      INS_SERIAL_NO = str_remove(INS_SERIAL_NO, "\\.0$"), #remove ".0" from strings; these are unexplainably introduced when importing directly from google drive
      `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss` = str_replace(`DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss`, "T", " "), #remove the "T" in the middle
      `RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss` = str_replace(`RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss`, "T", " "),
      `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss` = as.POSIXct(`DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss`, format = "%Y-%m-%d %H:%M:%S", tz = timezone), #convert to posixct
      `RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss` = as.POSIXct(`RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss`, format = "%Y-%m-%d %H:%M:%S", tz = timezone)
    ) %>%
    rename(
      'Receiver ID' = INS_SERIAL_NO,
      'Date In' = `DEPLOY_DATE_TIME...yyyy-mm-ddThh:mm:ss`,
      'Date Out' = `RECOVER_DATE_TIME.yyyy-mm-ddThh:mm:ss`
    ) %>%
    arrange(
      STATION_NO,
      "Date In"
    ) %>%
    distinct()

  return(rec.mov)
}

#-------------------------------------------------------------------------------
# Function: extract_tag_metadata
#-------------------------------------------------------------------------------
#' @description Extracts tag metadata from the catch data.
#' @param catch_data The catch data frame.
#' @param timezone The timezone to use for converting date/time values.
#' @return A data frame containing tag metadata.
extract_tag_metadata <- function(catch_data, timezone) {
  ind.attr <- catch_data %>%
    arrange(
      acoustic_tag_id,
      event_dt
    ) %>%
    filter(
      !(is.na(acoustic_tag_id)) #filter out non-acoustic ids
      # !tag_owner %in% "FIU/Yannis Papastamatiou" #these are both Yannis's/Tristan's tags: shared project
    ) %>%
    mutate(
      # Acoustic_Tag_ID = str_replace(Acoustic_Tag_ID, "\\-.*",""), #only take the first characters preceeding the "-"
      across(c(pcl, fl, tl, stl), ~ na_if(., "xxx")), #remove char string from columns
      time = suppressWarnings(format(as.POSIXct(as.numeric(event_ts) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")),
      time = ifelse(is.na(time), "Invalid Time", time),
      tagging_datetime = as.POSIXct(paste(event_dt, time), format = "%Y-%m-%d %H:%M", tz = timezone),
      suppressWarnings(across(c("pcl","fl","tl","stl"), as.numeric))
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
      across(acoustic_tag_id), .keep_all = TRUE #removes duplicates which arise from re-entering acoustic ids when tagged shark gets recaptured
    ) %>%
    arrange(
      species,
      acoustic_tag_id
    )
  return(ind.attr)
}

#-------------------------------------------------------------------------------
# Function: process_detection_files
#-------------------------------------------------------------------------------
#' @description Processes acoustic detection files from a Google Drive folder.
#' @param folder_path The path to the Google Drive folder.
#' @return A data frame containing the processed detection data.
process_detection_files <- function(folder_path) {
  # List all CSV files in the specified folder
  csv_files <- drive_ls(path = folder_path, pattern = "\\.csv$", recursive = TRUE)

  # Download and read all CSV files
  myfiles <- map(csv_files$id, function(file_id) {
    temp_file <- tempfile(fileext = ".csv")
    drive_download(file = as_id(file_id), path = temp_file, overwrite = TRUE)

    df <- read_csv(temp_file,
                   col_types = cols(.default = "c")
    ) #read all columns as character

    unlink(temp_file) #delete the temporary file

    return(df)
  })

  # Process each dataframe
  myfiles <- map(myfiles, function(df) {
    df %>%
      select(`Date and Time (UTC)`, Receiver, Transmitter, `Sensor Value`, `Sensor Unit`) %>%
      rename(
        time = `Date and Time (UTC)`,
        station = Receiver,
        elasmo = Transmitter,
        sensor.value = `Sensor Value`,
        sensor.unit = `Sensor Unit`
      ) %>%
      mutate(
        station = str_replace(station, "VR2W-", ""), #remove receiver string
        elasmo = str_replace(elasmo, "A69-9001-|A69-9006-|A69-1602-|A69-9002-|A69-1601-|A69-1303-", ""), #remove tag string
        time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC"),
        elasmo = as.numeric(elasmo),
        agency = "STB (BAH)"
      ) %>%
      arrange(
        time
      )
  })

  # Combine all data frames
  raw.det <- bind_rows(myfiles)
  return(raw.det)
}

#-------------------------------------------------------------------------------
# Main Script Logic
#-------------------------------------------------------------------------------

# 1. Import Data
otn_short <- import_data_from_gdrive(
  folder_url = "https://drive.google.com/drive/folders/1kShVtR3it9WUlVg9L4HzFcNA9R2_LYrN",
  file_pattern = "otn-instrument-deployment-short-form_GUTTRIDGE_2024_SEPT 24_1.xlsx",
  sheet = 2,
  col_types = c("INS_SERIAL_NO" = "text")
)

catch <- import_data_from_gdrive(
  folder_url = "https://drive.google.com/drive/folders/1LzoZdCqBDhpYQEBc6gb-M2zBdFgKnQop",
  file_pattern = "SharkCapture.xlsx",
  sheet = 1
)

# 2. Clean Column Names
otn_short <- clean_column_names(otn_short)

# 3. Define excluded receiver stations that were only deployed temporarily
excluded_stations <- c("SOM1", "SOM2", "BBC1", "MB2", "MB3", "MB4", "MB5", "BWC", "GCC",
                       "Deep Drop 2", "Salvador", "Deep Drop 1", "Bightbackreef", "On Buoy")

# 4. Extract Metadata
rec.attr <- extract_receiver_metadata(otn_short, excluded_stations)
rec.mov  <- extract_receiver_deployment_data(otn_short, excluded_stations, data_timezone)
ind.attr <- extract_tag_metadata(catch, data_timezone)

# 5. Process Detection Files
detection_folder_path <- "https://drive.google.com/drive/folders/1oE72VHV4L_Gwm5zk48eEtOSBhMvzu99q"
raw.det <- process_detection_files(detection_folder_path)

# 6. Save Data
data_files <- list(
  VLOC_STB = rec.attr,
  VMOV = rec.mov,
  IND = ind.attr,
  DET = raw.det
)

purrr::iwalk(data_files, ~saveRDS(.x, here::here("data", paste0(.y, ".rds"))))

# 7. Clean up environment
rm(catch, otn_short, excluded_stations)





