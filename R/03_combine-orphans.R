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
# rm(list = ls())

# Load Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(here, tidyverse, googledrive)

# Global Configuration
# config <- list(
#   data_timezone = "US/Eastern",
#   data_directory = here::here("data"),
#   data_files = list(rec_attr = "vloc.rds", andr_det = "det_compiled.rds"),
#   # Define point of contact mapping
#   poc_mapping = c(
#     "Olivia Dixon (liv@beneaththewaves.org)" = "BTW (BAH)",
#     "Joy Young (joy.young@myfwc.com)" = "FWC-TEQ (FL-Atl)",
#     "Jeffery Merrell (jhmerrel@ncsu.edu)" = "NCSU (NC)",
#     "Lucas Griffin (lucaspgriffin@gmail.com)" = "BTT (FL-Keys)",
#     "Sue Lowerre-Barbieri (GOM)" = "UF/FWC (FL-GOM)",
#     "Sue Lowerre-Barbieri (FLK)" = "UF/FWC (FL-Keys)",
#     "Michael Dance" = "LSU (LA)",
#     "Kate Choate (kate.choate@noaa.gov)" = "NOAA (VA)",
#     "Alejandro Acosta (alejandro.acosta@myfwc.com), Danielle Morley (danielle.morley@myfwc.com)" = "FWC (FL-Keys)",
#     "Shawn Harper (shawn.harper@ncaquriums.com), Nancy PhamHo (nancy.phamho@sezarc.com)" = "NC Aq. (NC)",
#     "Wilmelie Cruz-Marrero (wilmelie.cruz@noaa.gov)" = "NOAA (VA)",
#     "Brendan Runde (brendan.runde@tnc.org)" = "TNC (VA)",
#     "Brian Gervelis (brian@inspireenvironmental.com), Jeff Kneebone (jkneebone@neaq.org)" = "INSPIRE Env. (MA)",
#     "Edward Kim (ekim@neaq.org), Jeff Kneebone (jkneebone@neaq.org)" = "New England Aq. (MA)",
#     "Keith Dunton (kdunton@monmouth.edu)" = "Monmouth Uni (NJ)",
#     "Will Patterson (ATL)" = "UF (FL-Atl)",
#     "Will Patterson (GOM)" = "UF (FL-GOM)"
#   ),
#   google_drive = list(
#     otn_folder = "https://drive.google.com/drive/folders/1yoSVIIgJvZOigLv90xnIvqtSP_O_bweT",
#     private_folder = "https://drive.google.com/drive/folders/19P4_s0gTFSw9mp1u6-1rPaisE5SJNGZR",
#     file_pattern = "\\.csv$"
#   )
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
# Function: import_data_from_google_drive
#-------------------------------------------------------------------------------
#' @description Lists, downloads, and reads CSV files from a Google Drive folder.
#' @param config_list List of configuration parameters.
#' @param folder_url URL of the Google Drive folder.
#' @param file_pattern Pattern to match the CSV files.
#' @return A list of data frames containing the contents of the CSV files.
import_data_from_google_drive <- function(config_list, folder_url, file_pattern) {
  tryCatch(
    {
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
    },
    error = function(e) {
      cat("Error importing data from Google Drive:", e$message, "\n")
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: import_and_map_orphan_data
#-------------------------------------------------------------------------------
#' @description Imports orphan detection files from Google Drive, maps
#' contact points to agencies, and performs initial data cleaning.
#' @param config_list List of configuration parameters.
#' @param folder_url URL of the Google Drive folder containing the data.
#' @param file_pattern Pattern to match the CSV files in the folder.
#' @param poc_mapping Named vector mapping contact points to agencies.
#' @param timezone Timezone for the data.
#' @param excluded_locations (Optional) A vector of locations to exclude
#' (Default: NULL).
#' @return A data frame containing the combined and mapped orphan detections.
import_orphan_data <- function(config_list, folder_url, file_pattern, poc_mapping,
                               timezone, excluded_locations = NULL) {
  # Input validation: Check if excluded_locations is a character vector
  if (!is.null(excluded_locations) && !is.character(excluded_locations)) {
    stop("excluded_locations must be a character vector or NULL.")
  }

  # Import data using the new function
  myfiles <- import_data_from_google_drive(config_list, folder_url, file_pattern)

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
        "deploy_lat" = latitude,
        "deploy_long" = longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
          lubridate::with_tz(timezone), # Convert UTC to local timezone
        location = str_replace_all(
          location,
          c(
            "BUOY" = "buoy",
            "buoyBACKREEF" = "Buoybackreef"
          )
        ), # Consistency
        elasmo = str_replace(
          elasmo,
          "A69-(9001|9006|1602|9002|1601|1303)-",
          ""
        ), # Remove tag prefixes
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
      {
        if (!is.null(excluded_locations)) filter(., !location %in% excluded_locations) else .
      } %>%
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
#' @param config_list List of configuration parameters.
#' @param folder_url URL of the Google Drive folder containing the data.
#' @param file_pattern Pattern to match the CSV files in the folder.
#' @param poc_mapping Named vector mapping contact points to agencies.
#' @param timezone Timezone for the data.
#' @param excluded_locations (Optional) A vector of locations to exclude.
#' Defaults to NULL.
#' @return A data frame containing the combined and processed privately sent
#'   orphan detections.
import_orphan_data_private <- function(config_list, folder_url, file_pattern, poc_mapping,
                                       timezone, excluded_locations = NULL) {
  # Input validation: Check if excluded_locations is a character vector
  if (!is.null(excluded_locations) && !is.character(excluded_locations)) {
    stop("excluded_locations must be a character vector or NULL.")
  }

  # Import data using the new function
  myfiles <- import_data_from_google_drive(config_list, folder_url, file_pattern)

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
        "deploy_lat" = Latitude,
        "deploy_long" = Longitude
      ) %>%
      mutate(
        time = as.POSIXct(time, format = "%d/%m/%y %H:%M", tz = "UTC"),
        time = with_tz(time, timezone), # Convert UTC to local timezone
        station = str_replace(station, "VR2AR-", ""),
        elasmo = str_replace(
          elasmo,
          "A69-9001-|A69-9006-|A69-1602-|A69-9002-|A69-1601-|A69-1303-",
          ""
        ),
        sensor_value = NA,
        sensor_unit = NA,
        agency = case_when( # Match point of contact to right agency
          contact_poc %in% names(poc_mapping) ~ poc_mapping[contact_poc],
          TRUE ~ "" # default value if not in the mapping
        )
      ) %>%
      # Conditionally filter out excluded locations
      {
        if (!is.null(excluded_locations)) filter(., !location %in% excluded_locations) else .
      } %>%
      select( # change order to match the previous
        time,
        station,
        elasmo,
        sensor_value,
        sensor_unit,
        location,
        deploy_lat,
        deploy_long,
        agency,
        -contact_poc # Remove the contact_poc column
      ) %>%
      distinct() # ensures any duplicates are filtered out
  }) %>%
    bind_rows() # Combine all data frames
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
      deploy_long = first(deploy_long[which.max(str_count(deploy_long, "\\d"))]),
      deploy_lat = first(deploy_lat[which.max(str_count(deploy_lat, "\\d"))])
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
    select(deploy_long, deploy_lat, location, agency) %>%
    mutate(
      deploy_long = as.numeric(deploy_long),
      deploy_lat = as.numeric(deploy_lat),
      instrument_depth = case_when(
        location == "rd west - acoustic release" ~ 178,
        location == "rd east - acoustic release" ~ 182,
        location == "rd north - acoustic release" ~ 153,
        location == "dart3" ~ 148,
        TRUE ~ NA_real_
      )
    ) %>%
    distinct(deploy_long, deploy_lat, .keep_all = TRUE)

  # Bind and add more metadata
  rec_attr %>%
    bind_rows(unique_orph_loc) %>%
    mutate(
      bottom_depth = case_when(
        is.na(bottom_depth) & location == "RD WEST - ACOUSTIC RELEASE" ~ 178,
        is.na(bottom_depth) & location == "RD EAST - ACOUSTIC RELEASE" ~ 182,
        is.na(bottom_depth) & location == "BIG ROCK" ~ 100.5,
        TRUE ~ bottom_depth
      )
    )
}

#-------------------------------------------------------------------------------
# Function: process_all_data
#-------------------------------------------------------------------------------
#' @description Imports, cleans, and merges orphan detection data with
#' receiver attributes.
#' @param config_new A list containing the new configuration parameters for
#' data processing.
#' @return None. The function saves processed data to RDS files as a side effect.
process_all_data <- function(config_list) {
  # Load Data
  loaded_data <- purrr::map(config_new$data_files, ~ readRDS(file.path(config_new$data_directory, .x)))
  rec_attr <- loaded_data$rec_attr
  andr_det <- loaded_data$andr_det

  # Authenticate with Google Drive
  authenticate_drive()

  # Import and Process Orphan Data
  orph_otn <- import_orphan_data(
    config_list,
    folder_url = config_list$google_drive$otn_folder,
    file_pattern = config_list$google_drive$file_pattern,
    poc_mapping = config_list$poc_mapping,
    timezone = config_list$data_timezone
  ) # OTN Matched Detections

  orph_priv <- import_orphan_data_private(
    config_list,
    folder_url = config_list$google_drive$private_folder,
    file_pattern = config_list$google_drive$file_pattern,
    poc_mapping = config_list$poc_mapping,
    timezone = config_list$data_timezone
  ) # Privately Sent Orphan Detections

  # Combine and Process Orphan Detections
  orph <- bind_rows(orph_otn, orph_priv)
  orph$location <- str_replace_all(str_squish(str_to_lower(orph$location)), "\\s", "_") # Converts strings to lowercase, trim leading/trailing whitespace
  # and reduce multiple internal spaces to single spaces, replace all remaining spaces with underscores
  orph <- correct_gps_coordinates(orph) # Correct inconsistent GPS coordinates
  orph$elasmo <- as.numeric(orph$elasmo)
  rec_attr <- update_receiver_attributes(rec_attr, orph) # Update receiver attributes
  orph <- select(orph, -deploy_long, -deploy_lat) # Match the columns of orph to those of andr.det

  # Merge All Detections
  det <- bind_rows(andr_det, orph)

  # Define data files for saving
  data_to_save <- list(
    vloc = rec_attr,
    det_tot = det
  )

  # Save data files using iwalk
  purrr::iwalk(data_to_save, ~ saveRDS(.x, file.path(config_list$data_directory, paste0(.y, ".rds"))))

  cat("Script completed successfully.\n")
}
