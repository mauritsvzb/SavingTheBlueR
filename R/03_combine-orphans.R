#-------------------------------------------------------------------------------
# Script: Extract and Integrate Orphan Acoustic Detections
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-02-28
#
# Description:
# This script extracts 'orphan' acoustic detections (detections of our
# transmitters on other arrays) from OTN Matched Detections, combines them,
# adds point of contact information, integrates them with the main detection
# data, and updates the receiver attribute file.
#
# Dependencies:
# - tidyverse
# - googledrive
# - lubridate
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, googledrive, lubridate)

# Function Definitions

#-------------------------------------------------------------------------------
# Function: import_data_from_google_drive
#-------------------------------------------------------------------------------
#' @description Lists, downloads, and reads CSV files from a Google Drive folder.
#' @param folder_url URL of the Google Drive folder.
#' @param file_pattern Pattern to match the CSV files.
#' @return A list of data frames containing the contents of the CSV files.
import_data_from_google_drive <- function(folder_url, file_pattern) {
  # List all CSV files in the specified
  csv_files <- drive_ls(path = folder_url, pattern = file_pattern, recursive = TRUE)

  # Download and read all CSV files
  purrr::map(csv_files$id, function(file_id) {
    temp_file <- tempfile(fileext = ".csv")
    drive_download(file = as_id(file_id), path = temp_file, overwrite = TRUE)

    df <- read_csv(temp_file, col_types = cols(.default = "c"))

    unlink(temp_file)
    return(df)
  })
}

#-------------------------------------------------------------------------------
# Function: import_and_map_orphan_data
#-------------------------------------------------------------------------------
#' @description Imports orphan detection files from Google Drive, maps
#' contact points to agencies, and performs initial data cleaning.
#' @param folder_url URL of the Google Drive folder containing the data.
#' @param file_pattern Pattern to match the CSV files in the folder.
#' @param poc_mapping Named vector mapping contact points to agencies.
#' @param timezone Timezone for the data.
#' @return A data frame containing the combined and mapped orphan detections.
import_and_map_orphan_data <- function(folder_url, file_pattern, poc_mapping, timezone) {
  # Import data using the new function
  myfiles <- import_data_from_google_drive(folder_url, file_pattern)

  # Process each data frame
  purrr::map(myfiles, function(df) {
    df %>%
      select(
        datecollected,
        receiver,
        tagname,
        sensorvalue,
        sensorunit,
        station,
        longitude,
        latitude,
        contact_poc
      ) %>%
      rename(
        "time" = datecollected,
        "station" = receiver,
        "elasmo" = tagname,
        "sensor.value" = sensorvalue,
        "sensor.unit" = sensorunit,
        "location" = station,
        "GPS.N" = latitude,
        "GPS.W" = longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
          lubridate::with_tz(tzone = timezone), # Convert UTC to local timezone
        location = str_replace_all(location, c("BUOY" = "buoy", "buoyBACKREEF" = "Buoybackreef")), # Consistency
        elasmo = str_replace(elasmo, "A69-(9001|9006|1602|9002|1601|1303)-", ""), # Remove tag prefixes
        agency = case_when( # Agency from contact point
          contact_poc %in% names(poc_mapping) ~ poc_mapping[contact_poc],
          TRUE ~ "" # Default if unknown
        )
      ) %>%
      filter(
        station != "release", # Remove spurious detections
        location != "BIGHTBACKREEF", # Remove retired receiver
        agency != "" # Remove detections without assigned agency
      ) %>%
      select(-contact_poc) %>% # Remove contact point column
      distinct() # Remove duplicates
  }) %>%
    bind_rows() # Combine all data frames
}

#-------------------------------------------------------------------------------
# Function: import_orphan_data_private
#-------------------------------------------------------------------------------
#' @description Imports privately sent orphan detection files from Google Drive
#' and performs initial data cleaning.
#' @param folder_url URL of the Google Drive folder containing the data.
#' @param file_pattern Pattern to match the CSV files in the folder.
#' @param timezone Timezone for the data.
#' @return A data frame containing the combined and processed privately sent
#'   orphan detections.
import_orphan_data_private <- function(folder_url, file_pattern, timezone) {
  # Import data using the new function
  myfiles <- import_data_from_google_drive(folder_url, file_pattern)

  # Process each data frame
  purrr::map(myfiles, function(df) {
    df %>%
      select(
        `Date and Time (UTC)`,
        Receiver,
        Transmitter,
        `Station Name`,
        Latitude,
        Longitude,
        contact_poc
      ) %>%
      rename(
        "time" = `Date and Time (UTC)`,
        "station" = Receiver,
        "elasmo" = Transmitter,
        "location" = `Station Name`,
        "GPS.N" = Latitude,
        "GPS.W" = Longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%d/%m/%y %H:%M", tz = "UTC"),
        time = with_tz(time, tzone = timezone), # Convert UTC to local timezone
        station = str_replace(station, "VR2AR-", ""),
        elasmo = str_replace(elasmo, "A69-9001-|A69-9006-|A69-1602-|A69-9002-|A69-1601-|A69-1303-",""),
        sensor.value = NA,
        sensor.unit = NA,
        agency = case_when( # Match point of contact to right agency as per definitions above
          contact_poc %in% names(poc_mapping) ~ poc_mapping[contact_poc],
          TRUE ~ "" #default value if not in the mapping
        )
      ) %>%
      select( #change order to match the previous
        time,
        station,
        elasmo,
        sensor.value,
        sensor.unit,
        location,
        GPS.N,
        GPS.W,
        agency,
        -contact_poc # Remove the contact_poc column
      ) %>%
      distinct() #ensures any duplicates are filtered out
  }) %>%
    bind_rows() # Combine all data frames
}

#-------------------------------------------------------------------------------
# Function: clean_strings
#-------------------------------------------------------------------------------
#' @description Cleans a vector of strings by converting to lowercase,
#' removing leading/trailing whitespace, and collapsing multiple spaces.
#' @param strings A character vector.
#' @return A cleaned character vector.
clean_strings <- function(strings) {
  strings %>%
    tolower() %>%
    str_trim() %>%
    str_replace_all("\\s+", " ")
}

#-------------------------------------------------------------------------------
# Function: correct_gps_coordinates
#-------------------------------------------------------------------------------
#' @description Corrects inconsistent GPS coordinates by selecting the coordinate
#' with the most digits after the decimal.
#' @param df A data frame containing GPS coordinates.
#' @return A data frame with corrected GPS coordinates.
correct_gps_coordinates <- function(df) {
  df %>%
    group_by(location) %>%
    mutate(
      GPS.W = first(GPS.W[which.max(str_count(GPS.W, "\\d"))]),
      GPS.N = first(GPS.N[which.max(str_count(GPS.N, "\\d"))])
    ) %>%
    ungroup()
}

#-------------------------------------------------------------------------------
# Function: update_receiver_attributes
#-------------------------------------------------------------------------------
#' @description Updates the receiver attributes with orphan locations.
#' @param rec_attr Data frame containing receiver attributes.
#' @param orph Data frame containing orphan detections.
#' @return An updated receiver attributes data frame.
update_receiver_attributes <- function(rec_attr, orph) {
  # Extract unique combinations from orphan data
  unique_orph_loc <- orph %>%
    select(GPS.W, GPS.N, location, agency) %>%
    mutate(
      GPS.W = as.numeric(GPS.W),
      GPS.N = as.numeric(GPS.N),
      INSTRUMENT_DEPTH = case_when(
        location == "rd west - acoustic release" ~ 178,
        location == "rd east - acoustic release" ~ 182,
        location == "rd north - acoustic release" ~ 153,
        location == "dart3" ~ 148,
        TRUE ~ NA_real_
      )
    ) %>%
    distinct(GPS.W, GPS.N, .keep_all = TRUE)

  # Bind and add more metadata
  rec_attr %>%
    bind_rows(unique_orph_loc) %>%
    mutate(
      BOTTOM_DEPTH = case_when(
        is.na(BOTTOM_DEPTH) & location == "RD WEST - ACOUSTIC RELEASE" ~ 178,
        is.na(BOTTOM_DEPTH) & location == "RD EAST - ACOUSTIC RELEASE" ~ 182,
        is.na(BOTTOM_DEPTH) & location == "BIG ROCK" ~ 100.5,
        TRUE ~ BOTTOM_DEPTH
      )
    )
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------

# Define Global Variables/Constants
data_timezone <- "US/Eastern"
data_directory <- here::here("data")

# Define the list of files to load
data_files <- list(
  rec.attr = "VLOC_STB.rds",
  andr.det = "DET_compiled.rds"
)

# Load Data
loaded_data <- purrr::map(data_files, ~readRDS(file.path(data_directory, .x)))


# Assign loaded data to variables
rec.attr <- loaded_data$rec.attr
andr.det <- loaded_data$andr.det

# Define point of contact mapping
poc_mapping <- c(
  "Olivia Dixon (liv@beneaththewaves.org)" = "BTW (BAH)",
  "Joy Young (joy.young@myfwc.com)" = "FWC-TEQ (FL-Atl)",
  "Jeffery Merrell (jhmerrel@ncsu.edu)" = "NCSU (NC)",
  "Lucas Griffin (lucaspgriffin@gmail.com)" = "BTT (FL-Keys)",
  "Sue Lowerre-Barbieri" = "UF (FL GOM)",
  "Michael Dance" = "LSU (LA)"
)

# Google Drive Authentication
drive_auth()

# OTN Matched Detections
folder_path_otn <- "https://drive.google.com/drive/folders/1yoSVIIgJvZOigLv90xnIvqtSP_O_bweT"
file_pattern_otn <- "\\.csv$"
orph.otn <- import_and_map_orphan_data(folder_path_otn, file_pattern_otn, poc_mapping, data_timezone)

# Privately Sent Orphan Detections
folder_path_priv <- "https://drive.google.com/drive/folders/19P4_s0gTFSw9mp1u6-1rPaisE5SJNGZR"
file_pattern_priv <- "\\.csv$"
orph.priv <- import_orphan_data_private(folder_path_priv, file_pattern_priv, data_timezone)

# Combine orphan detections
orph <- bind_rows(orph.otn, orph.priv)

# Clean up location strings
orph$location <- clean_strings(orph$location)

# Correct inconsistent GPS coordinates
orph <- correct_gps_coordinates(orph)

# Update receiver attributes
rec.attr <- update_receiver_attributes(rec.attr, orph)

# Match the columns of orph to those of andr.det
orph <- select(orph, -GPS.W, -GPS.N)

# Convert orph elasmo data type
orph$elasmo <- as.numeric(orph$elasmo)

# Merge all detections together
det <- bind_rows(andr.det, orph)

# Define data files for saving
data_files_save <- list(
  VLOC_STB = rec.attr,
  DET_tot = det
)

# Save data files using iwalk
purrr::iwalk(data_files_save, ~saveRDS(.x, file.path(data_directory, paste0(.y, ".rds"))))

cat("Script completed successfully.\n")
