#-------------------------------------------------------------------------------
# Script: Abacus Plot Generator for Acoustic Telemetry Data
# Author: Maurits van Zinnicq Bergmann
# Date:   2025-03-16
#
# Description:
# Generates an abacus plot to visualize acoustic telemetry detections over time,
# incorporating individual shark IDs, sex, size (STL), and tagging locations.
# The script supports species-specific filtering and customization of plot aesthetics.
#
# Dependencies:
# - tidyverse
#-------------------------------------------------------------------------------

# Fresh Start
rm(list = ls())

# Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, tidyverse, viridis)

# Function Definitions

#-------------------------------------------------------------------------------
# Function: load_and_prepare_data
#-------------------------------------------------------------------------------
#' @description Loads and prepares acoustic telemetry data for plotting.
#' @param config List containing configuration parameters (see Configuration Section).
#' @param exclude_site (Optional) Site(s) to exclude e.g., "Jupiter Florida" (Default: NULL)
#' @param filter_species (Optional) Species to filter for e.g., "Carcharhinus perezii" (Default: NULL)
#' @return A data frame containing the prepared data, or NULL if an error occurs.
load_and_prepare_data <- function(config, exclude_site = NULL, filter_species = NULL) {
  tryCatch({
    # Load data'
    det_cleaned <- readRDS(file.path(config$data_directory, "det_cleaned.rds"))
    ind <- readRDS(file.path(config$data_directory, "ind.rds"))
    vloc <- readRDS(file.path(config$data_directory, "vloc.rds"))

    # Data cleaning and transformation
    df <- det_cleaned %>%
      arrange(time) %>%
      mutate(elasmo = as.numeric(elasmo))

    colnames(ind)[1] <- "elasmo"
    ind <- ind %>%
      mutate(elasmo = as.numeric(elasmo))

    # Exclude specified site(s) if provided in config
    if (!is.null(exclude_site)) {
      ind <- ind %>%
        filter(!site %in% exclude_site)
    }

    df1 <- df %>%
      inner_join(ind, by = "elasmo")

    df2 <- df1 %>%
      left_join(vloc, by = "location") %>%
      rename(agency = agency.x) %>%
      select(
        time,
        station,
        elasmo,
        agency,
        location,
        tagging_datetime,
        species,
        sex,
        pcl,
        fl,
        tl,
        stl,
        latitude,
        longitude
      )

    # Filter by species if specified in config
    if (!is.null(filter_species)) {
      df2 <- filter(df2, species == filter_species)
    }

    return(df2)

  }, error = function(e) {
    message("Error loading or preparing data: ", e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: filter_by_time_window
#-------------------------------------------------------------------------------
#' @description Filters the data by a specified time window.
#' @param df Data frame to filter.
#' @param config List containing configuration parameters, including start_time and end_time.
#' @return Filtered data frame, or NULL if an error occurs.
filter_by_time_window <- function(df, config) {
  tryCatch({
    # Check if start_time and end_time are in config
    if (is.null(config$start_time) || is.null(config$end_time)) {
      message("start_time and/or end_time not specified in config; returning full dataset.")
      return(df)  # Return original data if time window not specified
    }

    # Convert start and end times to POSIXct with correct time zone
    start_time <- as.POSIXct(config$start_time, tz = config$dat.TZ)
    end_time <- as.POSIXct(config$end_time, tz = config$dat.TZ)

    # Check if start_time and end_time are valid POSIXct objects
    if (is.na(start_time) || is.na(end_time)) {
      stop("Invalid start_time or end_time format in config.")
    }

    # Filter data by time window
    filtered_df <- df %>%
      filter(time >= start_time & time <= end_time)

    return(filtered_df)

  }, error = function(e) {
    message("Error filtering by time window: ", e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: calculate_time_bins
#-------------------------------------------------------------------------------
#' @description Calculates time bins for the abacus plot.
#' @param df Data frame containing the detection data.
#' @param config List containing configuration parameters (see Configuration Section).
#' @return A list containing the time sequence and the data frame with added bin information.
calculate_time_bins <- function(df, config) {
  tryCatch({
    # Calculate time range
    min.time <- min(df$time)
    min.time <- format(min.time, "%Y-%m-%d")
    min.time <- as.POSIXct(min.time, "%Y-%m-%d", tz = config$dat.TZ)

    max.time <- max(df$time)
    max.time <- format(max.time, "%Y-%m-%d", tz = config$dat.TZ)
    max.time <- as.POSIXct(max.time, tz = config$dat.TZ)

    # Create time sequence
    time.int <- difftime(max.time, min.time - (3600 * 24 * 30), units = config$timeint)
    lo <- ceiling(as.numeric(time.int)) / config$time
    timeseq <- seq(
      from = min.time - (3600 * 24 * 30 * 7),
      length.out = (lo + (0.4 * lo)),
      by = paste(config$time, config$timeint, sep = " ")
    )
    timeseq <- as.POSIXct(format(timeseq, "%Y-%m-%d"), tz = config$dat.TZ) # Convert to date-only POSIXct

    # Assign time bins to detections
    df <- df %>%
      mutate(bin = as.factor(findInterval(time, timeseq)))

    return(list(timeseq = timeseq, df = df))

  }, error = function(e) {
    message("Error calculating time bins: ", e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: group_agencies_by_country
#-------------------------------------------------------------------------------
#' Group agencies by country
#' @description This function groups agencies into countries and arranges the data frame.
#' @param df A data frame containing acoustic telemetry data.
#' @return A modified data frame with a new 'country' column and arranged by species, elasmo, and time.
group_agencies_by_country <- function(df) {
  df %>%
    mutate(
      country = case_when(
        agency %in% c("STB (BAH)", "BTW (BAH)") ~ "Bahamas",
        agency %in% c("FWC-TEQ (FL-Atl)", "BTT (FL-Keys)", "UF (FL-GOM)", "FWC (FL-Keys)") ~ "FL, USA",
        agency %in% c("NCSU (NC)", "NC Aq. (NC)") ~ "NC, USA",
        agency %in% c("LSU (LA)") ~ "LA, USA",
        agency %in% c("NOAA (VA)", "TNC (VA)") ~ "VA, USA",
        agency %in% c("INSPIRE Env. (MA)", "New England Aq. (MA)") ~ "MA, USA",
        agency %in% c("Monmouth Uni (NJ)") ~ "NJ, USA",
        TRUE ~ NA_character_  # For any unmatched cases
      )
    ) %>%
    arrange(
      species,
      elasmo,
      time
    )
}

#-------------------------------------------------------------------------------
# Function: map_agency_colors_and_country_shapes
#-------------------------------------------------------------------------------
#' @description Maps agency affiliations to colors and country to shapes for plotting.
#' @param df Data frame containing detection data.
#' @param config List containing configuration parameters (see Configuration Section).
#' @return The data frame with added 'tagging_date_color' (color) and 'tagging_date_symbol' (shape) columns.
map_agency_colors_and_country_shapes <- function(df, color_palette = NULL, shapes = NULL) {
  tryCatch({
    # Get unique agencies and countries
    agencies <- df %>% pull(agency) %>% unique()
    countries <- df %>% pull(country) %>% unique()

    # Ensure color palette and shapes are sufficient
    if (length(config$color_palette) < length(agencies)) {
      stop(paste0(
        "Color palette in config is too short for the number of agencies. ",
        "Please specify at least ", length(agencies), " colors."
      ))
    }
    if (length(config$shapes) < length(countries)) {
      stop(paste0(
        "Shapes vector in config is too short for the number of countries. ",
        "Please specify at least ", length(countries), " shapes."
      ))
    }

    # Generate colors if not provided
    if (is.null(color_palette)) {
      color_palette <- viridis(n = length(agencies), option = "viridis")
    } else
      # Assign black to STB if present
      if ("STB (BAH)" %in% agencies) {
        stb_index <- which(agencies == "STB (BAH)")
        color_palette[stb_index] <- "#000000"
      }

    # Generate shapes if not provided
    if (is.null(shapes)) {
      shapes <- 15:(14 + length(countries))
    } else
      if ("Bahamas" %in% countries) {
        bahamas_index <- which(countries == "Bahamas")

        # If shape 16 is already used, find its index
        existing_16_index <- which(shapes == 16)

        if (length(existing_16_index) > 0 &&
            existing_16_index != bahamas_index) {
          # Assign the next available shape to the country that had 16
          available_shapes <- setdiff(15:25, shapes)
          shapes[existing_16_index] <- if (length(available_shapes) > 0)
            min(available_shapes)
          else
            26
        }

        # Assign shape 16 to Bahamas
        shapes[bahamas_index] <- 16
      }

    # Create mappings
    color_mapping <- tibble(
      agency = agencies,
      color = color_palette[1:length(agencies)]
      )

    shape_mapping <- tibble(
      country = countries,
      shape = shapes[1:length(countries)]
      )

    # Merge with dataframe
    df %>%
      left_join(color_mapping, by = "agency") %>%
      rename(tagging_date_color = color) %>%
      left_join(shape_mapping, by = "country") %>%
      rename(tagging_date_symbol = shape)
  }, error = function(e) {
    message("Error in map_agency_colors_and_country_shapes: ",
            e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: process_tagging_data
#-------------------------------------------------------------------------------
#' @description Processes tagging data to add tagging dates to the abacus plot.
#' @param df Data frame containing detection data with color and shape mappings.
#' @param ind Data frame containing tagging information.
#' @param timeint Vector of time intervals used for binning detection data.
#' @param config List containing configuration parameters (see Configuration Section).
#' @return A data frame containing tagging data with date and aesthetics for plotting.
process_tagging_data <- function(df, ind, timeint, config) {
  tryCatch({
    # Create indice column
    taglist <- unique(df$elasmo)
    df <- df %>%
      mutate(indice = match(elasmo, taglist))

    # Create date column
    time_num <- seq_along(timeseq)
    date_df <- data.frame(bin = time_num, date = timeseq)

    df <- df %>%
      mutate(bin = as.integer(as.character(bin))) %>%
      left_join(date_df, by = "bin") %>%
      mutate(bin = as.factor(bin))

    # Prepare tagging data
    ind <- readRDS(file.path(config$data_directory, "ind.rds"))
    tag.up <- ind %>%
      rename(elasmo = acoustic_tag_id) %>%
      filter(elasmo %in% df$elasmo) %>%
      select(elasmo, species, sex, stl, tagging_datetime) %>%
      arrange(species, elasmo)

    # Create temporary data frame
    temp <- data.frame(matrix(nrow = nrow(tag.up), ncol = ncol(df), NA))
    names(temp) <- names(df)

    # Populate with relevant tagging information
    temp[, "elasmo"] <- tag.up$elasmo
    temp[, "sex"] <- tag.up$sex
    temp[, "stl"] <- tag.up$stl
    temp[, "tagging_datetime"] <- tag.up$tagging_datetime
    temp[, "species"] <- tag.up$species
    temp[, "tagging_date_color"] <- config$tagging_date_color
    temp[, "tagging_date_symbol"] <- config$tagging_date_symbol

    # Transform tagging date to the same scale as the abacus plot i.e. to appropriate time interval bins
    date.t <- sapply(temp$tagging_datetime, function(tagging_date) {
      format(
        max(
          timeseq[which(timeseq <= tagging_date)]
        ),
        "%Y-%m-%d %H:%M:%S")
    })

    # replace original tagging date
    temp$date <- as.POSIXct(date.t,"%Y-%m-%d %H:%M:%S", tz = config$dat.TZ)

    # tag id order inherited from tag.up
    temp <- temp %>%
      mutate(indice = match(elasmo, taglist))

    return(list(df = df, temp = temp))

  }, error = function(e) {
    message("Error processing tagging data: ", e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Function: create_abacus_plot
#-------------------------------------------------------------------------------
#' @description Generates the abacus plot.
#' @param df Data frame containing detection data with aesthetics.
#' @param temp Data frame containing tagging data with aesthetics.
#' @param taglist Vector of unique tag IDs.
#' @param config List containing configuration parameters (see Configuration Section).
create_abacus_plot <- function(df, temp, taglist, config) {
  tryCatch({
    # Open TIFF file for output
    tiff(
      filename = file.path(config$output_directory, config$output_filename),
      width = config$image_width,
      height = config$image_height,
      units = "mm",
      res = config$plot_resolution
    )

    # Set plot margins and clipping
    par(mar = c(5.1, 13, 4.1, 3), xpd = T)

    # Create the plot
    plot(
      df$date, df$indice,
      xlim = c((min(df$date) - months(5)), max(df$date)),
      cex = rep(config$default_point_size, length(df$indice)),
      col = as.character(df$tagging_date_color),
      pch = df$tagging_date_symbol,
      yaxt = "n", xaxt = "n", ann = F
    )

    # Draw horizontal segments for each tag ID
    # Get the plot region coordinates
    usr <- par("usr")
    left_border <- usr[1]
    right_border <- usr[2]
    for (y in 1:length(unique(df$elasmo))) {
      segments(left_border, y, right_border, y, lty = 3, col = "grey", lwd = .7)
    }

    # Set vertical axes labels: Tag IDs, Sex, and STL
    # tag.up <- temp %>%
    #   select(elasmo, sex, stl) %>%
    #   distinct() %>%
    #   arrange(elasmo)

    axis(side = 2, at = seq(1, length(taglist), 1),
         labels = taglist, cex.axis = config$vertical_axis_label_cex, las = 2) # sets the primary y-axis label, tag IDs
    axis(side = 2, line = 2.0, at = 1:length(taglist), labels = temp$sex, las = 2, cex.axis = config$vertical_axis_label_cex, tick = F) # sets secondary y-axis label: Sex
    axis(side = 2, line = 3.0, at = 1:length(taglist), labels = temp$stl, las = 2, cex.axis = config$vertical_axis_label_cex, tick = F) # sets tertiary y-axis label: STL

    # Set horizontal axes labels: Months and Years
    axis.POSIXct(1, at = seq((min(df$date) - months(6)), max(df$date), by = "month") - 1, format = "%b",
                 las = 2, cex.axis = config$horizontal_axis_label_cex) # sets primary x axis label, months
    axis.POSIXct(1, line = 1.2, at = seq(as.POSIXct("2011-01-01 00:00:00", tz = config$dat.TZ), max(df$date), by = "year"), format = "%Y",
                 las = 0, cex.axis = config$horizontal_axis_label_cex, tick = F) # sets secondary x axis label: year

    # Add Tagging Dates
    points(temp$date, temp$indice, col = temp$tagging_date_color, pch = temp$tagging_date_symbol)

    # Create the legend
    tpch <- df %>%
      select(agency, country, tagging_date_color, tagging_date_symbol) %>%
      distinct() %>%
      arrange(country)

    # Add hierarchical labeling to show groups:
    unique_species <- unique(temp$species)

    # Create a vector of positions for each unique species
    species_positions <- tapply(1:length(temp$species), temp$species, min)

    # Add the labels
    mtext(side = 2, text = unique_species,
          at = species_positions,
          line = config$species_label_line, las = 2, cex = 0.8)

    # add legend to the plot
    legend(
      "topleft",
      inset = config$legend_inset,
      legend = tpch$agency,
      pch = tpch$tagging_date_symbol,
      col = as.character(tpch$tagging_date_color),
      cex = config$legend_text_cex,
      text.width = max(strwidth(tpch$agency, units = "user", cex = 0.8)),
      y.intersp = 0.9, # Adjust this value to control vertical spacing
      bty = config$legend_box
    )

    dev.off()

  }, error = function(e) {
    message("Error creating abacus plot: ", e$message)
    return(NULL)
  })
}

#-------------------------------------------------------------------------------
# Main Script Execution
#-------------------------------------------------------------------------------
# Global Configuration
config <- list(
  data_directory = here::here("data"), # Main project data directory
  output_directory = here::here("output"), # Directory to save plots and intermediate files
  dat.TZ = "US/Eastern", # Time zone of data
  timeint = "day", # Time interval unit
  time = 4, # Time resolution of data points (in days)
  sep = ",", # Separator for CSV files
  dec = ".", # Decimal separator
  exclude_site = NULL, # (OPTIONAL) Filter for tagging site
  filter_species = NULL, # (OPTIONAL) Filter for species
  start_time = NULL, # In local time zone e.g., "2018-01-01 00:00:00"
  end_time = NULL, # In local time zone e.g., "2020-12-31 23:59:59"
  color_palette = c( # (OPTIONAL) Color palette to use,
    "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
    "#CC6677", "#882255", "#AA4499", "#661100", "#6699CC", "#AA4466", "#4477AA"),
  shapes = c( # (OPTIONAL) Shape mapping to use,
                    16, 17, 15, 18, 8, 7, 6),
  output_filename = "abacus.tiff", # Name of the output TIFF file
  image_width = 340, # Width of the output image in mm
  image_height = 300, # Height of the output image in mm
  plot_resolution = 300, # Resolution of the output image in DPI
  tagging_date_color = "red", # Color for tagging date points
  tagging_date_symbol = "|", # Shape for tagging date points
  default_point_size = 0.8, # Default size for detection points
  species_label_line = 5.5, # Distance of species labels from the axis
  legend_inset = c(0.01, 0.01), # Inset for the legend position
  legend_text_cex = 0.7, # Legend text size
  legend_box = "y", # Show legend box
  vertical_axis_label_cex = 0.7, # Vertical axis labels size
  horizontal_axis_label_cex = 0.7 # Horizontal axis labels size
)

# Load and Prepare Data
data <- load_and_prepare_data(
  config,
  exclude_site = config$exclude_site,
  filter_species = config$filter_species
)
if (is.null(data)) {
  stop("Failed to load and prepare data.")
}

# Filter by Time Window
data <- filter_by_time_window(data, config)
if (is.null(data)) {
  stop("Failed to filter by time window.")
}

# Calculate Time Bins
time_bins_result <- calculate_time_bins(data, config)
if (is.null(time_bins_result)) {
  stop("Failed to calculate time bins.")
}
timeseq <- time_bins_result$timeseq
df <- time_bins_result$df

# Group Agencies by Country (and state, if applicable)
df <- group_agencies_by_country(df)

# Map Agency Colors and Country Shapes
df <- map_agency_colors_and_country_shapes(df, color_palette = config$color_palette, shapes = config$shapes)
if (is.null(df)) {
  stop("Failed to map agency colors and country shapes.")
}
# Get taglist AFTER data preparation and time bin calculation
taglist <- unique(df$elasmo)

# Process Tagging Data
processed_data <- process_tagging_data(df, ind, timeseq, config)
if (is.null(processed_data)) {
  stop("Failed to process tagging data.")
}
df <- processed_data$df
temp <- processed_data$temp

# 5. Create Abacus Plot
create_abacus_plot(df, temp, taglist, config)
message("Abacus plot generated successfully.")
