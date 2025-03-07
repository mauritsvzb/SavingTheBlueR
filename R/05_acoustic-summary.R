#-------------------------------------------------------------------------------
# Script: Generate Acoustic Telemetry Data Summaries
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-07
#
# Description:
# This script generates seasonal summaries of acoustic detection data including:
# - Individual animal movement statistics
# - Detection matrices by location/agency
# - Temporal analysis of detection patterns
#
# Dependencies:
# - tidyverse
# - lubridate
#-------------------------------------------------------------------------------

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})
