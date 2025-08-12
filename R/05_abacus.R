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
# rm(list = ls())

# Load Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(here, tidyverse, viridis)

# Global Configuration
# config_list <- list(
#   data_directory = here::here("data"), # Main project data directory
#   output_directory = here::here("output"), # Directory to save plots and intermediate files
#   dat.TZ = "US/Eastern", # Time zone of data
#   timeint = "day", # Time interval unit
#   time = 4, # Time resolution of data points (in days)
#   sep = ",", # Separator for CSV files
#   dec = ".", # Decimal separator
#   exclude_site = NULL, # (OPTIONAL) Filter for tagging site; In this case this was done in L200
#   filter_species = NULL, # (OPTIONAL) Filter for species; In this case this was done in L199
#   start_time = NULL, # In local time zone e.g., "2018-01-01 00:00:00"
#   end_time = NULL, # In local time zone e.g., "2020-12-31 23:59:59"
#   color_palette = c( # (OPTIONAL) Color palette to use,
#     "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#6C7B8B",
#     "#CC6677", "#882255", "#AA4499", "#661100", "#6699CC", "#AA4466", "#4477AA",
#     "#C37241", "#495649", "#885E93"),
#   shapes = c( # (OPTIONAL) Shape mapping to use,
#     16, 17, 15, 18, 8, 7, 6, 0, 3, 4),
#   output_filename = "abacus.tiff", # Name of the output TIFF file
#   image_width = 550, # Width of the output image in mm
#   image_height = 400, # Height of the output image in mm
#   plot_resolution = 250, # Resolution of the output image in DPI
#   tagging_date_color = "red", # Color for tagging date points
#   tagging_date_symbol = "|", # Shape for tagging date points
#   default_point_size = 0.8, # Default size for detection points
#   species_labels = TRUE, # Whether species labels should be used
#   species_label_line = 5.5, # Distance of species labels from the axis
#   include_legend = TRUE, # Whether you want to show a legend
#   legend_inset = c(0.01, 0.01), # Inset for the legend position
#   legend_text_cex = 0.7, # Legend text size
#   legend_box = "y", # Show legend box
#   xlim_months_left = 7, # Number of months to extend left of min date on x-axis
#   vertical_axis_label_cex = 0.7, # Vertical axis labels size
#   horizontal_axis_label_cex = 0.7 # Horizontal axis labels size
# )

# Function Definitions

#-------------------------------------------------------------------------------
# Function: load_and_prepare_data
#-------------------------------------------------------------------------------
#' @description Loads and prepares acoustic telemetry data for plotting.
#' @param config_list List containing configuration parameters (see Configuration Section).
#' @param exclude_site (Optional) Site(s) to exclude e.g., "Jupiter Florida" (Default: NULL)
#' @param filter_species (Optional) Species to filter for e.g., "Carcharhinus perezii" (Default: NULL)
#' @return A data frame containing the prepared data, or NULL if an error occurs.
load_and_prepare_data <- function(config_list, exclude_site = NULL, filter_species = NULL) {
  tryCatch(
    {
      # Load data
      det_cleaned <- readRDS(file.path(config_list$data_directory, "det_cleaned.rds"))
      ind <- readRDS(file.path(config_list$data_directory, "ind.rds"))
      vloc <- readRDS(file.path(config_list$data_directory, "vloc.rds"))

      # Data cleaning and transformation
      df <- det_cleaned %>%
        arrange(time) %>%
        mutate(elasmo = as.numeric(elasmo))

      colnames(ind)[1] <- "elasmo"
      ind <- ind %>%
        mutate(elasmo = as.numeric(elasmo))

      # Exclude specified site(s) if provided in config_list
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

      # Filter by species if specified in config_list
      if (!is.null(filter_species)) {
        df2 <- filter(df2, species == filter_species)
      }

      return(df2)
    },
    error = function(e) {
      message("Error loading or preparing data: ", e$message)
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: filter_by_time_window
#-------------------------------------------------------------------------------
#' @description Filters the data by a specified time window.
#' @param config_list List containing configuration parameters, including start_time and end_time.
#' @param df Data frame to filter.
#' @return Filtered data frame, or NULL if an error occurs.
filter_by_time_window <- function(config_list, df, timezone) {
  tryCatch(
    {
      # Check if start_time and end_time are in config_list
      if (is.null(config_list$start_time) || is.null(config_list$end_time)) {
        message("start_time and/or end_time not specified in config; returning full dataset.")
        return(df) # Return original data if time window not specified
      }

      # Convert start and end times to POSIXct with correct time zone
      start_time <- as.POSIXct(config_list$start_time, tz = timezone)
      end_time <- as.POSIXct(config_list$end_time, tz = timezone)

      # Check if start_time and end_time are valid POSIXct objects
      if (is.na(start_time) || is.na(end_time)) {
        stop("Invalid start_time or end_time format in config.")
      }

      # Filter data by time window
      filtered_df <- df %>%
        filter(time >= start_time & time <= end_time)

      return(filtered_df)
    },
    error = function(e) {
      message("Error filtering by time window: ", e$message)
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: calculate_time_bins
#-------------------------------------------------------------------------------
#' @description Calculates time bins for the abacus plot.
#' @param config_list List containing configuration parameters (see Configuration Section).
#' @param df Data frame containing the detection data.
#' @return A list containing the time sequence and the data frame with added bin information.
calculate_time_bins <- function(config_list, df, timezone) {
  tryCatch(
    {
      # Calculate time range
      min.time <- min(df$time)
      min.time <- format(min.time, "%Y-%m-%d", tz = config_list$dat.TZ)
      min.time <- as.POSIXct(min.time, "%Y-%m-%d", tz = config_list$dat.TZ)

      max.time <- max(df$time)
      max.time <- format(max.time, "%Y-%m-%d", tz = config_list$dat.TZ)
      max.time <- as.POSIXct(max.time, "%Y-%m-%d", tz = config_list$dat.TZ)

      # Create time sequence
      time.int <- difftime(max.time, min.time - (3600 * 24 * 30 * config_list$xlim_months_left), units = config_list$timeint)
      lo <- ceiling(as.numeric(time.int)) / config_list$time
      timeseq <- seq(
        from = min.time - (3600 * 24 * 30 * config_list$xlim_months_left),
        length.out = (lo + (0.4 * lo)),
        by = paste(config_list$time, config_list$timeint, sep = " ")
      )
      timeseq <- as.POSIXct(format(timeseq, "%Y-%m-%d"), tz = timezone) # Convert to date-only POSIXct

      # Assign time bins to detections
      df <- df %>%
        mutate(bin = as.factor(findInterval(time, timeseq)))

      return(list(timeseq = timeseq, df = df))
    },
    error = function(e) {
      message("Error calculating time bins: ", e$message)
      return(NULL)
    }
  )
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
        agency %in% c("BTT (FL-Keys)", "FWC (FL-Keys)", "FWC-TEQ (FL-Atl)", "UF (FL-Atl)",
                      "UF/FWC (FL-GOM)", "UF/FWC (FL-Keys)") ~ "FL, USA",
        agency %in% c("NCSU (NC)", "NC Aq. (NC)") ~ "NC, USA",
        agency %in% c("LSU (LA)") ~ "LA, USA",
        agency %in% c("NOAA (VA)", "TNC (VA)") ~ "VA, USA",
        agency %in% c("INSPIRE Env. (MA)", "New England Aq. (MA)") ~ "MA, USA",
        agency %in% c("Monmouth Uni (NJ)") ~ "NJ, USA",
        agency %in% c("NOAA (GOM)") ~ "USA",
        TRUE ~ NA_character_ # For any unmatched cases
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
#' @param config_list List containing configuration parameters (see Configuration Section).
#' @param df Data frame containing detection data.
#' @return The data frame with added 'tagging_date_color' (color) and 'tagging_date_symbol' (shape) columns.
map_agency_colors_and_country_shapes <- function(config_list, df, color_palette = NULL, shapes = NULL) {
  tryCatch(
    {
      # Get unique agencies and countries
      agencies <- df %>%
        pull(agency) %>%
        unique()
      countries <- df %>%
        pull(country) %>%
        unique()

      # Ensure color palette and shapes are sufficient
      if (length(config_list$color_palette) < length(agencies)) {
        stop(paste0(
          "Color palette in config_list is too short for the number of agencies. ",
          "Please specify at least ", length(agencies), " colors."
        ))
      }
      if (length(config_list$shapes) < length(countries)) {
        stop(paste0(
          "Shapes vector in config_list is too short for the number of countries. ",
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
      } else if ("Bahamas" %in% countries) {
        bahamas_index <- which(countries == "Bahamas")

        # If shape 16 is already used, find its index
        existing_16_index <- which(shapes == 16)

        if (length(existing_16_index) > 0 &&
          existing_16_index != bahamas_index) {
          # Assign the next available shape to the country that had 16
          available_shapes <- setdiff(15:25, shapes)
          shapes[existing_16_index] <- if (length(available_shapes) > 0) {
            min(available_shapes)
          } else {
            26
          }
        }

        # Assign shape 16 to Bahamas
        shapes[bahamas_index] <- 16
      }

      # Create mappings
      color_mapping <- tibble(
        agency = agencies,
        data_point_color = color_palette[1:length(agencies)]
      )

      shape_mapping <- tibble(
        country = countries,
        data_point_shape = shapes[1:length(countries)]
      )

      # Merge with dataframe
      df %>%
        left_join(color_mapping, by = "agency") %>%
        left_join(shape_mapping, by = "country")
    },
    error = function(e) {
      message(
        "Error in map_agency_colors_and_country_shapes: ",
        e$message
      )
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: process_tagging_data
#-------------------------------------------------------------------------------
#' @description Processes tagging data to add tagging dates to the abacus plot.
#' @param config_list List containing configuration parameters (see Configuration Section).
#' @param df Data frame containing detection data with color and shape mappings.
#' @param timeseq Vector of time intervals used for binning detection data.
#' @param timezone The timezone for the data.
#' @return A data frame containing tagging data with date and aesthetics for plotting.
process_tagging_data <- function(config_list, df, timeseq, timezone) {
  tryCatch(
    {
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
      ind <- readRDS(file.path(config_list$data_directory, "ind.rds"))
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
      temp[, "tagging_date_color"] <- config_list$tagging_date_color
      temp[, "tagging_date_symbol"] <- config_list$tagging_date_symbol

      # Transform tagging date to the same scale as the abacus plot i.e. to appropriate time interval bins
      date.t <- sapply(temp$tagging_datetime, function(tagging_date) {
        format(
          max(
            timeseq[which(timeseq <= tagging_date)]
          ),
          "%Y-%m-%d %H:%M:%S"
        )
      })

      # replace original tagging date
      temp$date <- as.POSIXct(date.t, "%Y-%m-%d %H:%M:%S", tz = timezone)

      # tag id order inherited from tag.up
      temp <- temp %>%
        mutate(indice = match(elasmo, taglist))

      return(list(df = df, temp = temp))
    },
    error = function(e) {
      message("Error processing tagging data: ", e$message)
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Function: create_abacus_plot
#-------------------------------------------------------------------------------
#' @description Generates the abacus plot.
#' @param config_list List containing configuration parameters (see Configuration Section).
#' @param df Data frame containing detection data with aesthetics.
#' @param temp Data frame containing tagging data with aesthetics.
#' @param taglist Vector of unique tag IDs.
#' @param timezone The timezone for the data.
#' @param include_species_labels Whether species labels should be printed along the y axis (Default: FALSE)
#' @param include_legend Whether to include a legend in the plot
#' @param xlim_months_left Integer. Number of months to extend the x-axis to the left of the earliest date (default: 0).
abacus_plot <- function(config_list, df, temp, taglist, timezone, include_species_labels = TRUE, include_legend = TRUE, xlim_months_left = 0) {
  tryCatch(
    {
      # Open TIFF file for output
      tiff(
        filename = file.path(config_list$output_directory, config_list$output_filename),
        width = config_list$image_width,
        height = config_list$image_height,
        units = "mm",
        res = config_list$plot_resolution
      )

      # Set plot margins and clipping
      par(mar = c(5.1, 13, 4.1, 3), xpd = T)

      # Create the plot
      plot(
        df$date, df$indice,
        xlim = c((min(df$date) - months(xlim_months_left)), max(df$date)),
        cex = rep(config_list$default_point_size, length(df$indice)),
        col = as.character(df$data_point_color),
        pch = df$data_point_shape,
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

      axis(
        side = 2, at = seq(1, length(taglist), 1),
        labels = taglist, cex.axis = config_list$vertical_axis_label_cex, las = 2
      ) # sets the primary y-axis label, tag IDs
      axis(side = 2, line = 2.0, at = 1:length(taglist), labels = temp$sex, las = 2, cex.axis = config_list$vertical_axis_label_cex, tick = F) # sets secondary y-axis label: Sex
      axis(side = 2, line = 3.0, at = 1:length(taglist), labels = temp$stl, las = 2, cex.axis = config_list$vertical_axis_label_cex, tick = F) # sets tertiary y-axis label: STL

      # Conditional hierarchical labeling to show species groups
      if (include_species_labels) {
        # Inventorize species contained in data set
        unique_species <- unique(temp$species)

        # Create a vector of positions for each unique species
        species_positions <- tapply(1:length(temp$species), temp$species, min)

        # Add the labels
        mtext(
          side = 2, text = unique_species,
          at = species_positions,
          line = config_list$species_label_line, las = 2, cex = 0.8
        )
      }

      # Set horizontal axes labels: Months and Years
      # Calculate the first day of the month following the minimum date
      first_of_next_month <- ceiling_date(min(df$date), "month")

      axis.POSIXct(1,
        at = seq(first_of_next_month - (3600 * 24 * 30 * config_list$xlim_months_left), max(df$date), by = "month"), format = "%b",
        las = 2, cex.axis = 0.7
      ) # sets primary x axis label, months
      axis.POSIXct(1,
        line = 1.2, at = seq(as.POSIXct("2011-01-01 00:00:00", tz = timezone), max(df$date), by = "year"), format = "%Y",
        las = 0, cex.axis = 0.7, tick = F
      ) # sets secondary x axis label: year

      # Add Tagging Dates
      points(temp$date, temp$indice, col = temp$tagging_date_color, pch = temp$tagging_date_symbol)

      # Add legend conditionally
      if (include_legend) {
        tpch <- df %>%
          select(agency, country, data_point_color, data_point_shape) %>%
          distinct() %>%
          arrange(country)

        legend(
          "topleft",
          inset = config_list$legend_inset,
          legend = tpch$agency,
          pch = tpch$data_point_shape,
          col = as.character(tpch$data_point_color),
          cex = config_list$legend_text_cex,
          text.width = max(strwidth(tpch$agency, units = "user", cex = 0.8)),
          y.intersp = 0.9, # Adjust this value to control vertical spacing
          bty = config_list$legend_box
        )
      }

      dev.off()
    },
    error = function(e) {
      message("Error creating abacus plot: ", e$message)
      return(NULL)
    }
  )
}

#-------------------------------------------------------------------------------
# Main Function: generate_abacus_plot
#-------------------------------------------------------------------------------
#' @description
#'   This function orchestrates the generation of an abacus plot from acoustic
#'   telemetry data. It loads, prepares, filters, and processes the data before
#'   creating the final plot.
#' @param config_list
#'   List containing configuration parameters. This list should include paths
#'   to data files, time zone information, plot settings, and optional filters.
#'   See the example configuration for required parameters.
#' @details
#'   The function relies on several sub-functions for data loading, filtering,
#'   processing, and plotting. See the documentation for these functions for
#'   more details on their specific operations.
#'
#'   \itemize{
#'     \item{\code{load_and_prepare_data()}}{: Loads and prepares acoustic
#'     telemetry data for plotting.}
#'     \item{\code{filter_by_time_window()}}{: Filters the data by a specified
#'     time window.}
#'     \item{\code{calculate_time_bins()}}{: Calculates time bins for the
#'     abacus plot.}
#'     \item{\code{group_agencies_by_country()}}{: Groups agencies into
#'     countries.}
#'     \item{\code{map_agency_colors_and_country_shapes()}}{: Maps agency
#'     affiliations to colors and country to shapes for plotting.}
#'     \item{\code{process_tagging_data()}}{: Processes tagging data to add
#'     tagging dates to the abacus plot.}
#'     \item{\code{create_abacus_plot()}}{: Generates the abacus plot.}
#'   }
#'
#'   The \code{config_list} should contain the following elements:
#'
#'   \itemize{
#'     \item{\code{data_directory}}{: Path to the directory containing data
#'     files ("det_cleaned.rds", "ind.rds", "vloc.rds").}
#'     \item{\code{output_directory}}{: Directory where the output plot will be
#'     saved.}
#'     \item{\code{dat.TZ}}{: Time zone of the data.}
#'     \item{\code{timeint}}{: Time interval unit (e.g., "day").}
#'     \item{\code{time}}{: Time resolution of data points (in days).}
#'     \item{\code{sep}}{: Separator for CSV files.}
#'     \item{\code{dec}}{: Decimal separator.}
#'     \item{\code{exclude_site}}{: (Optional) Site(s) to exclude.}
#'     \item{\code{filter_species}}{: (Optional) Species to filter for.}
#'     \item{\code{start_time}}{: (Optional) Start time for filtering data.}
#'     \item{\code{end_time}}{: (Optional) End time for filtering data.}
#'     \item{\code{color_palette}}{: (Optional) Color palette to use.}
#'     \item{\code{shapes}}{: (Optional) Shape mapping to use.}
#'     \item{\code{output_filename}}{: Name of the output TIFF file.}
#'     \item{\code{image_width}}{: Width of the output image in mm.}
#'     \item{\code{image_height}}{: Height of the output image in mm.}
#'     \item{\code{plot_resolution}}{: Resolution of the output image in DPI.}
#'     \item{\code{tagging_date_color}}{: Color for tagging date points.}
#'     \item{\code{tagging_date_symbol}}{: Shape for tagging date points.}
#'     \item{\code{default_point_size}}{: Default size for detection points.}
#'     \item{\code{species_label_line}}{: Distance of species labels from the
#'     axis.}
#'     \item{\code{legend_inset}}{: Inset for the legend position.}
#'     \item{\code{legend_text_cex}}{: Legend text size.}
#'     \item{\code{legend_box}}{: Show legend box.}
#'     \item{\code{vertical_axis_label_cex}}{: Vertical axis labels size.}
#'     \item{\code{horizontal_axis_label_cex}}{: Horizontal axis labels size.}
#'   }
#'
#' @param config_list List of configuration parameters.
#' @return None.  Generates and saves a TIFF image of the abacus plot to the specified output directory.
generate_abacus_plot <- function(config_list) {
  # Load and Prepare Data
  data <- load_and_prepare_data(
    config_list,
    exclude_site = config_list$exclude_site,
    filter_species = config_list$filter_species
  )
  if (is.null(data)) {
    stop("Failed to load and prepare data.")
  }

  # Filter by Time Window
  data <- filter_by_time_window(
    config_list,
    df = data,
    timezone = config_list$dat.TZ
  )
  if (is.null(data)) {
    stop("Failed to filter by time window.")
  }

  # Calculate Time Bins
  time_bins_result <- calculate_time_bins(
    config_list,
    df = data,
    timezone = config_list$dat.TZ
  )
  if (is.null(time_bins_result)) {
    stop("Failed to calculate time bins.")
  }
  timeseq <- time_bins_result$timeseq
  df <- time_bins_result$df

  # Group Agencies by Country (and state, if applicable)
  df <- group_agencies_by_country(df)

  # Map Agency Colors and Country Shapes
  df <- map_agency_colors_and_country_shapes(
    config_list,
    df = df,
    color_palette = config_list$color_palette,
    shapes = config_list$shapes
  )
  if (is.null(df)) {
    stop("Failed to map agency colors and country shapes.")
  }

  # Get taglist AFTER data preparation and time bin calculation
  taglist <- unique(df$elasmo)

  # Process Tagging Data
  processed_data <- process_tagging_data(
    config_list,
    df = df,
    timeseq = timeseq,
    timezone = config_list$dat.TZ
  )
  if (is.null(processed_data)) {
    stop("Failed to process tagging data.")
  }
  df <- processed_data$df
  temp <- processed_data$temp

  # 5. Create Abacus Plot
  abacus_plot(
    config_list,
    df,
    temp,
    taglist,
    timezone = config_list$dat.TZ,
    include_species_labels = config_list$species_labels,
    include_legend = config_list$include_legend,
    xlim_months_left = config_list$xlim_months_left
  )

  message("Abacus plot generated successfully.")
}
