#-------------------------------------------------------------------------------
# Script: Compile and Filter Acoustic Detection Data
# Author: Sprinkles
# Date:   2025-02-24
#
# Description:
# This script compiles and filters acoustic detection data by:
# 1. Matching detections to tagged animals
# 2. Verifying detections occurred during receiver deployment periods
# 3. Ensuring detections are after tag deployment dates
#
# Dependencies:
# - tidyverse
# - lubridate
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, lubridate)

# Define Global Variables/Constants
data_timezone <- "US/Eastern"
data_directory <- here("data")

# Function Definitions

#-------------------------------------------------------------------------------
# Function: import_and_preprocess_data
#-------------------------------------------------------------------------------
#' @description Imports and preprocesses detection data, receiver movement data, and individual data
#' @param datadir Directory containing the data files
#' @param timezone The timezone for the data
#' @return A list containing the preprocessed dataframes
import_and_preprocess_data <- function(datadir, timezone) {
  # Import detection data
  det <- readRDS(file.path(datadir, "DET.rds")) %>%
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M", tz = "UTC") %>%
        with_tz(timezone),
      station = as.character(station),
      elasmo = as.character(elasmo)
    ) %>%
    arrange(elasmo, time)

  # Import receiver movement data
  VMOV <- readRDS(file.path(datadir, "VMOV.rds"))

  # Import individual data
  IND <- readRDS(file.path(datadir, "/IND.rds"))

  list(det = det, VMOV = VMOV, IND = IND)
}


#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------

# Run the compile function
compiled_data <- compile_data(dir, filter = FALSE)

# Print summary of compiled data
cat("Compilation complete. Summary of compiled data:\n")
print(summary(compiled_data))
