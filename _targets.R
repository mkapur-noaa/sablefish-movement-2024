# Load targets package
library(targets)

# Source scripts in R/
source("R/plot.R")
source("R/recenter.R")
source("R/utils.R")

# Set options
tar_option_set(
  packages = c(
    "dplyr",
    "fs",
    "ggplot2",
    "ggpubr",
    "ggsidekick",
    "ggspatial",
    "here",
    "magrittr",
    "mmmstan",
    "readr",
    "rgeos",
    "rlang",
    "rnaturalearth",
    "rnaturalearthdata",
    "sf",
    "tictoc",
    "tidyr",
    "usethis"
  )
)
options(tidyverse.quiet = TRUE)

# List target objects
list(
  # Define options -------------------------------------------------------------
  list(
    tar_target(figure_dpi, 600),
    tar_target(figure_ext, ".png")
  ),
  # Define years ---------------------------------------------------------------
  list(
    tar_target(year_start, 1979), # Year of first tag released
    tar_target(year_end, 2017), # Year of last tag recovered
    list()
  ),
  # Define regions -------------------------------------------------------------
  list(
    tar_target(
      list_regions_8,
      list(ai = 1, bs = 2, wg = 3, cg = 4, eg = 5, se = 6, bc = 7, cc = 8)
    ),
    tar_target(
      list_regions_3,
      list(ak = 1:6, bc = 7, cc = 8)
    ),
    tar_target(
      list_omregions_6,
      list(wak = 1, eak = 2, nbc = 3, sbc = 4, ncc = 5, scc = 6)
    ),
    list()
  ),
  # Define sizes ---------------------------------------------------------------
  list(
    tar_target(list_one_size, list(sml = 400:800)),
    tar_target(list_two_sizes, list(s = 400:549, l = 550:800)),
    list()
  ),
  # Define model step and term -------------------------------------------------
  list(
    tar_target(step_interval, "quarter"),
    tar_target(step_duration_max, 12L), # Max duration at large before recovery
    list()
  ),
  # Column name arguments ------------------------------------------------------
  list(
    tar_target(colname_date_released, "date_released"),
    tar_target(colname_date_recovered, "date_recovered"),
    tar_target(colname_region_released, "region_released"),
    tar_target(colname_region_recovered, "region_recovered"),
    tar_target(colname_size_released, "size_released"),
    # Operating model regions
    tar_target(colname_omregion_released, "omregion_released"),
    tar_target(colname_omregion_recovered, "omregion_recovered"),
    list()
  ),
  # Define CmdStanR arguments --------------------------------------------------
  list(
    tar_target(chains, 1L),
    tar_target(use_reduce_sum, TRUE), # TODO: Update to TRUE when implemented
    tar_target(refresh, 10)
  ),
  # Watch data -----------------------------------------------------------------
  list(
    tar_target(
      watch_tag_data,
      "data/tag_data.rda",
      format = "file"
    ),
    tar_target(
      watch_fishing_rate,
      "data/fishing_rate.rda",
      format = "file"
    ),
    tar_target(
      watch_abundance,
      "data/abundance.rda",
      format = "file"
    ),
    tar_target(
      watch_sf_regions,
      "data/sf_regions.rda",
      format = "file"
    ),
    tar_target(
      watch_sf_omregions,
      "data/sf_omregions.rda",
      format = "file"
    ),
    list()
  ),
  # Read data ------------------------------------------------------------------
  list(
    tar_target(tag_data, read_from_path(watch_tag_data)),
    tar_target(abundance, read_from_path(watch_abundance)),
    tar_target(sf_regions, read_from_path(watch_sf_regions)),
    tar_target(sf_omregions, read_from_path(watch_sf_omregions)),
    list()
  ),
  # Assemble fishing rate priors -----------------------------------------------
  list(
    # Fishing rate prior means for 8 regions
    tar_target(
      mu_fishing_rate_8,
      read_from_path(watch_fishing_rate) %>%
        dplyr::filter(.data$spatial == "region") %>%
        dplyr::filter(.data$year %in% c(year_start:year_end)) %>%
        dplyr::select(.data$year, .data$short, .data$fishing_rate) %>%
        dplyr::mutate(short = tolower(.data$short)) %>%
        tidyr::pivot_wider(
          names_from = .data$short,
          values_from = .data$fishing_rate
        ) %>%
        dplyr::select(-1L) %>%
        as.matrix()
    ),
    # Fishing rate prior means for 3 regions
    tar_target(
      mu_fishing_rate_3,
      read_from_path(watch_fishing_rate) %>%
        dplyr::filter(.data$spatial == "omregion") %>%
        dplyr::filter(.data$year %in% c(year_start:year_end)) %>%
        dplyr::select(.data$year, .data$short, .data$fishing_rate) %>%
        dplyr::mutate(short = tolower(.data$short)) %>%
        dplyr::filter(.data$short %in% c("wak", "nbc", "ncc")) %>%
        tidyr::pivot_wider(
          names_from = .data$short,
          values_from = .data$fishing_rate
        ) %>%
        dplyr::rename(ak = .data$wak, bc = .data$nbc, cc = .data$ncc) %>%
        dplyr::select(.data$ak, .data$bc, .data$cc) %>%
        as.matrix()
    ),
    # Fishing rate prior means for 6 omregions
    tar_target(
      mu_fishing_rate_6,
      read_from_path(watch_fishing_rate) %>%
        dplyr::filter(.data$spatial == "omregion") %>%
        dplyr::filter(.data$year %in% c(year_start:year_end)) %>%
        dplyr::select(.data$year, .data$short, .data$fishing_rate) %>%
        dplyr::mutate(short = tolower(.data$short)) %>%
        tidyr::pivot_wider(
          names_from = .data$short,
          values_from = .data$fishing_rate
        ) %>%
        dplyr::select(-1L) %>%
        as.matrix()
    ),
    tar_target(cv_fishing_rate, 0.1),
    list()
  ),
  # Plot map -------------------------------------------------------------------
  list(
    tar_target(
      map_regions,
      plot_map(
        regions = sf_regions,
        plot_name = "map-regions",
        size_short = 1.75,
        size_line = 0.25,
        size_text = 5,
        color_land = "white",
        color_ocean = "grey96",
        color_region = "grey30",
        fill_land = "white",
        fill_ocean = "grey95",
        fill_region = "grey85",
        xmin = 169,
        ymin = 31,
        xmax = 240.5,
        ymax = 65.5,
        width = 90,
        height = 65,
        dpi = figure_dpi,
        file_type = figure_ext
      ),
      format = "file"
    )
  ),
  # Fit regions 3 mean ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean,
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3,
        list_sizes = list_one_size,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released,
        colname_region_recovered = colname_region_recovered,
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL,
        movement_disallow = NULL,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3,
        cv_fishing_rate = cv_fishing_rate,
        # Reporting rate priors
        mu_reporting_rate = c(0.4, 0.5, 0.3),
        sd_reporting_rate = c(0.4, 0.5, 0.3) * 0.1,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 size ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_size,
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3,
        list_sizes = list_two_sizes, # Small and Large
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released,
        colname_region_recovered = colname_region_recovered,
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL,
        movement_disallow = NULL,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3,
        cv_fishing_rate = cv_fishing_rate,
        # Reporting rate priors
        mu_reporting_rate = c(0.4, 0.5, 0.3),
        sd_reporting_rate = c(0.4, 0.5, 0.3) * 0.1,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )
    )
  ),
  # Fit omregions 6 mean -------------------------------------------------------
  list(
    tar_target(
      mmmstan_omregions_6_mean,
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_omregions_6,
        list_sizes = list_one_size,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_omregion_released,
        colname_region_recovered = colname_omregion_recovered,
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL,
        movement_disallow = NULL,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_6,
        cv_fishing_rate = cv_fishing_rate,
        # Reporting rate priors
        mu_reporting_rate = c(0.4, 0.4, 0.5, 0.5, 0.3, 0.3),
        sd_reporting_rate = c(0.4, 0.4, 0.5, 0.5, 0.3, 0.3) * 0.1,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )
    )
  ),
  # Fit regions 8 mean ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_8_mean,
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_8,
        list_sizes = list_one_size,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released,
        colname_region_recovered = colname_region_recovered,
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = matrix(c(5, 7, 7, 5), nrow = 2, byrow = TRUE),
        movement_disallow = NULL,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_8,
        cv_fishing_rate = cv_fishing_rate,
        # Reporting rate priors
        mu_reporting_rate = c(rep(0.4, 6), 0.5, 0.3),
        sd_reporting_rate = c(rep(0.4, 6), 0.5, 0.3) * 0.1,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )
    )
  ),
  # Compute abundance exchange -------------------------------------------------
  list(
    tar_target(
      abundance_exchange,
      create_abundance_exchange(
        abundance,
        mmmstan_regions_3_mean$summary$movement_rate
      )
    ),
    list()
  ),
  # Plot heat regions 3 mean ---------------------------------------------------
  list(
    tar_target(
      heat_regions_3_mean,
      plot_heat(
        data = mmmstan_regions_3_mean$summary$movement_rate,
        plot_name = "heat-regions-3-mean",
        regions = toupper(names(list_regions_3)),
        size_text = 6,
        size_mean = 3,
        nudge_mean = 0.1,
        size_sd = 1.35,
        nudge_sd = 0.15,
        legend_name = "Movement rate",
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = 0,
        margin_y = 0,
        width = 90,
        height = 100,
        dpi = figure_dpi,
        file_type = figure_ext
      ),
      format = "file"
    )
  ),
  # Plot regions 3 bar size ----------------------------------------------------
  list(
    tar_target(
      bar_regions_3_size,
      plot_cols(
        data = mmmstan_regions_3_size$summary$movement_rate,
        plot_name = "bar-regions-3-size",
        regions = toupper(names(list_regions_3)),
        xvar = "l",
        xlab = "Length class",
        ylab = "Annual movement rate",
        x_text = c("Small", "Large"),
        x_breaks = 1:2,
        y_text = as.character(seq(0, 1, 0.25)),
        y_breaks = seq(0, 1, 0.25),
        x_angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        size_title = 8,
        size_strip = 8,
        size_text = 6,
        size_error = 0.3,
        panel_spacing = 1,
        xmin = NA,
        xmax = NA,
        ymin = 0.0,
        ymax = 1.0,
        width = 190,
        height = 170,
        dpi = figure_dpi,
        file_type = figure_ext
      )
    ),
    format = "file"
  ),
  # Plot heat omregions 6 mean -------------------------------------------------
  list(
    tar_target(
      heat_omregions_6_mean,
      plot_heat(
        data = mmmstan_omregions_6_mean$summary$movement_rate,
        plot_name = "heat-omregions-6-mean",
        regions = toupper(names(list_omregions_6)),
        size_text = 6,
        size_mean = 3,
        nudge_mean = 0.1,
        size_sd = 1.35,
        nudge_sd = 0.15,
        legend_name = "Movement rate",
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = 0,
        margin_y = 0,
        width = 90,
        height = 100,
        dpi = figure_dpi,
        file_type = figure_ext
      ),
      format = "file"
    )
  ),
  # Plot heat regions 8 mean ---------------------------------------------------
  list(
    tar_target(
      heat_regions_8_mean,
      plot_heat(
        data = mmmstan_regions_8_mean$summary$movement_rate,
        plot_name = "heat-regions-8-mean",
        regions = toupper(names(list_regions_8)),
        size_text = 6,
        size_mean = 3,
        nudge_mean = 0.1,
        size_sd = 1.35,
        nudge_sd = 0.15,
        legend_name = "Movement rate",
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = 0,
        margin_y = 0,
        width = 90,
        height = 100,
        dpi = figure_dpi,
        file_type = figure_ext
      ),
      format = "file"
    )
  ),
  # Plot abundance exchange ----------------------------------------------------
  list(
    tar_target(
      bar_abundance_exchange,
      plot_exchange(
        data = abundance_exchange,
        plot_name = "bar-abundance-exchange",
        size_hline = 0.5,
        size_error = 0.5,
        width = 190,
        height = 140,
        dpi = figure_dpi,
        file_type = figure_ext
      ),
      format = "file"
    )
  ),
  list()
)
