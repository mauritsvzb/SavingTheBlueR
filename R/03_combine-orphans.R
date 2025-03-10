#-------------------------------------------------------------------------------
# Script: Extract and Integrate Orphan Acoustic Detections
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-02-28
#
# Description:
# This script extracts 'orphan' acoustic detections (detections of our
# transmitters on other arrays) from' OTN Matched Detections', combines them,
# adds point of contact information so we can identify other arrays, integrates
# them with the local detection data, and updates the receiver attribute file.
#
# Dependencies:
# - tidyverse
# - googledrive
# - lubridate
#
# Notes:
# - Requires Google Drive authentication.
# - Designed to work with data created by the "Generate Detection and
#   Metadata Files" script.
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
  tryCatch({
    # List all CSV files in the specified folder
    csv_files <- drive_ls(path = folder_url, pattern = file_pattern, recursive = TRUE)

    # Check if any files were found
    if (nrow(csv_files) == 0) {
      stop("No CSV files found matching the specified pattern in the Google Drive folder.")
    }

    # Download and read all CSV files
    data_frames <- purrr::map(csv_files$id, function(file_id) {
      temp_file <- tempfile(fileext = ".csv")
      drive_download(file = as_id(file_id), path = temp_file, overwrite = TRUE)

      df <- read_csv(temp_file, col_types = cols(.default = "c"))

      unlink(temp_file)
      return(df)
    })
    return(data_frames)

  }, error = function(e) {
    cat("Error importing data from Google Drive:", e$message, "\n")
    return(NULL)
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
#' @param excluded_locations (Optional) A vector of locations to exclude
#' (Default: NULL).
#' @return A data frame containing the combined and mapped orphan detections.
import_orphan_data <- function(folder_url, file_pattern, poc_mapping,
                               timezone, excluded_locations = NULL) {

  # Input validation: Check if excluded_locations is a character vector
  if (!is.null(excluded_locations) && !is.character(excluded_locations)) {
    stop("excluded_locations must be a character vector or NULL.")
  }

  # Import data using the new function
  myfiles <- import_data_from_google_drive(folder_url, file_pattern)

  # Check if myfiles is NULL (error in import)
  if (is.null(myfiles)) {
    return(NULL) # Return NULL if import failed
  }
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
        "sensor_value" = sensorvalue,
        "sensor_unit" = sensorunit,
        "location" = station,
        "GPS_N" = latitude,
        "GPS_W" = longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
          lubridate::with_tz(timezone), # Convert UTC to local timezone
        location = str_replace_all(location,
                                   c("BUOY" = "buoy",
                                     "buoyBACKREEF" = "Buoybackreef")), # Consistency
        elasmo = str_replace(elasmo,
                             "A69-(9001|9006|1602|9002|1601|1303)-",
                             ""), # Remove tag prefixes
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
      # Conditionally filter out excluded locations
      {if (!is.null(excluded_locations)) filter(., !location %in% excluded_locations) else .} %>%
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
#' @param excluded_locations (Optional) A vector of locations to exclude.
#' Defaults to NULL.
#' @return A data frame containing the combined and processed privately sent
#'   orphan detections.
import_orphan_data_private <- function(folder_url, file_pattern, timezone, excluded_locations = NULL) {

  # Input validation: Check if excluded_locations is a character vector
  if (!is.null(excluded_locations) && !is.character(excluded_locations)) {
    stop("excluded_locations must be a character vector or NULL.")
  }

  # Import data using the new function
  myfiles <- import_data_from_google_drive(folder_url, file_pattern)

  # Check if myfiles is NULL (error in import)
  if (is.null(myfiles)) {
    return(NULL) # Return NULL if import failed
  }

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
        "GPS_N" = Latitude,
        "GPS_W" = Longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%d/%m/%y %H:%M", tz = "UTC"),
        time = with_tz(time, timezone), # Convert UTC to local timezone
        station = str_replace(station, "VR2AR-", ""),
        elasmo = str_replace(elasmo,
                             "A69-9001-|A69-9006-|A69-1602-|A69-9002-|A69-1601-|A69-1303-",
                             ""),
        sensor_value = NA,
        sensor_unit = NA,
        agency = case_when( # Match point of contact to right agency as per definitions above
          contact_poc %in% names(poc_mapping) ~ poc_mapping[contact_poc],
          TRUE ~ "" #default value if not in the mapping
        )
      ) %>%
      # Conditionally filter out excluded locations
      {if (!is.null(excluded_locations)) filter(., !location %in% excluded_locations) else .} %>%
      select( #change order to match the previous
        time,
        station,
        elasmo,
        sensor_value,
        sensor_unit,
        location,
        GPS_N,
        GPS_W,
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
#' @description Corrects inconsistent GPS coordinates by selecting the
#' coordinate with the most digits after the decimal.
#' @param df A data frame containing GPS coordinates.
#' @return A data frame with corrected GPS coordinates.
correct_gps_coordinates <- function(df) {
  df %>%
    group_by(location) %>%
    mutate(
      GPS_W = first(GPS_W[which.max(str_count(GPS_W, "\\d"))]),
      GPS_N = first(GPS_N[which.max(str_count(GPS_N, "\\d"))])
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
    select(GPS_W, GPS_N, location, agency) %>%
    mutate(
      GPS_W = as.numeric(GPS_W),
      GPS_N = as.numeric(GPS_N),
      INSTRUMENT_DEPTH = case_when(
        location == "rd west - acoustic release" ~ 178,
        location == "rd east - acoustic release" ~ 182,
        location == "rd north - acoustic release" ~ 153,
        location == "dart3" ~ 148,
        TRUE ~ NA_real_
      )
    ) %>%
    distinct(GPS_W, GPS_N, .keep_all = TRUE)

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

# Global Configuration
data_timezone <- "US/Eastern"
data_directory <- here::here("data")

# Define the list of files to load
data_files <- list(
  rec_attr = "vloc_stb.rds",
  andr_det = "det_compiled.rds"
)

# Load Data
loaded_data <- purrr::map(data_files, ~readRDS(file.path(data_directory, .x)))

# Assign loaded data to variables
rec_attr <- loaded_data$rec_attr
andr_det <- loaded_data$andr_det

# Define point of contact mapping
poc_mapping <- c(
  "Olivia Dixon (liv@beneaththewaves.org)" = "BTW (BAH)",
  "Joy Young (joy.young@myfwc.com)" = "FWC-TEQ (FL-Atl)",
  "Jeffery Merrell (jhmerrel@ncsu.edu)" = "NCSU (NC)",
  "Lucas Griffin (lucaspgriffin@gmail.com)" = "BTT (FL-Keys)",
  "Sue Lowerre-Barbieri" = "UF (FL-GOM)",
  "Michael Dance" = "LSU (LA)",
  "Kate Choate (kate.choate@noaa.gov)" = "NOAA (VA)",
  "Alejandro Acosta (alejandro.acosta@myfwc.com), Danielle Morley (danielle.morley@myfwc.com)" = "FWC (FL-Keys)",
  "Shawn Harper (shawn.harper@ncaquriums.com), Nancy PhamHo (nancy.phamho@sezarc.com)" = "NC Aq. (NC)",
  "Wilmelie Cruz-Marrero (wilmelie.cruz@noaa.gov)" = "NOAA (VA)",
  "Brendan Runde (brendan.runde@tnc.org)" = "TNC (VA)",
  "Brian Gervelis (brian@inspireenvironmental.com), Jeff Kneebone (jkneebone@neaq.org)" = "INSPIRE Env. (MA)",
  "Edward Kim (ekim@neaq.org), Jeff Kneebone (jkneebone@neaq.org)" = "New England Aq. (MA)",
  "Keith Dunton (kdunton@monmouth.edu)" = "Monmouth Uni (NJ)"
)

# Google Drive Authentication
drive_auth()

# OTN Matched Detections
folder_path_otn <- "https://drive.google.com/drive/folders/1yoSVIIgJvZOigLv90xnIvqtSP_O_bweT"
file_pattern_otn <- "\\.csv$"
orph_otn <- import_orphan_data(folder_path_otn, file_pattern_otn, poc_mapping, data_timezone)

# Privately Sent Orphan Detections
folder_path_priv <- "https://drive.google.com/drive/folders/19P4_s0gTFSw9mp1u6-1rPaisE5SJNGZR"
file_pattern_priv <- "\\.csv$"
orph_priv <- import_orphan_data_private(folder_path_priv, file_pattern_priv, data_timezone)

# Combine orphan detections
orph <- bind_rows(orph_otn, orph_priv)

# Clean up location strings
orph$location <- clean_strings(orph$location)

# Correct inconsistent GPS coordinates
orph <- correct_gps_coordinates(orph)

# Update receiver attributes
rec_attr <- update_receiver_attributes(rec_attr, orph)

# Match the columns of orph to those of andr.det
orph <- select(orph, -GPS_W, -GPS_N)

# Convert orph elasmo data type
orph$elasmo <- as.numeric(orph$elasmo)

# Merge all detections together
det <- bind_rows(andr_det, orph)

# Define data files for saving
data_files_save <- list(
  vloc_stb = rec_attr,
  det_tot = det
)

# Save data files using iwalk
purrr::iwalk(data_files_save, ~saveRDS(.x, file.path(data_directory, paste0(.y, ".rds"))))

cat("Script completed successfully.\n")
