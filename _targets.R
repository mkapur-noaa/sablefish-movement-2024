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
  # Define figure options ------------------------------------------------------
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
      list_regions_3,
      list(ak = 1:6, bc = 7, cc = 8)
    ),
    tar_target(
      list_regions_6,
      list(wak = 1, eak = 2, nbc = 3, sbc = 4, ncc = 5, scc = 6)
    ),
    tar_target(
      list_regions_8,
      list(ai = 1, bs = 2, wg = 3, cg = 4, eg = 5, se = 6, bc = 7, cc = 8)
    )
  ),
  # Define sizes ---------------------------------------------------------------
  list(
    tar_target(list_sizes_1, list(sml = 400:800)),
    tar_target(list_sizes_2, list(s = 400:549, l = 550:800)),
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
  # Define movement step diagonal priors ---------------------------------------
  list(
    tar_target(mu_movement_step_diag_3, rep(0.98, 3)),
    tar_target(sd_movement_step_diag_3, rep(0.05, 3)),
    tar_target(mu_movement_step_diag_6, rep(0.98, 6)),
    tar_target(sd_movement_step_diag_6, rep(0.05, 6)),
    tar_target(mu_movement_step_diag_8, rep(0.98, 8)),
    tar_target(sd_movement_step_diag_8, rep(0.05, 8))
  ),
  # Define reporting rate priors -----------------------------------------------
  list(
    tar_target(mu_reporting_rate_3, c(0.4, 0.5, 0.3)),
    tar_target(sd_reporting_rate_3, c(0.4, 0.5, 0.3) * 0.1),
    tar_target(mu_reporting_rate_6, c(0.4, 0.4, 0.5, 0.5, 0.3, 0.3)),
    tar_target(sd_reporting_rate_6, c(0.4, 0.4, 0.5, 0.5, 0.3, 0.3) * 0.1),
    tar_target(mu_reporting_rate_8, c(rep(0.4, 6), 0.5, 0.3)),
    tar_target(sd_reporting_rate_8, c(rep(0.4, 6), 0.5, 0.3) * 0.1)
  ),
  # Define selectivity priors --------------------------------------------------
  list(
    tar_target(mu_selectivity_3, array(rep(c(0.9, 1), 3), dim = c(2, 3))),
    tar_target(mu_selectivity_6, array(rep(c(0.9, 1), 6), dim = c(2, 6))),
    tar_target(cv_selectivity, 0.1)
  ),
  # Define natural mortality rate priors ---------------------------------------
  list(
    tar_target(mu_natural_mortality_rate_3, rep(0.1, 3)),
    tar_target(sd_natural_mortality_rate_3, rep(0.01, 3)),
    tar_target(mu_natural_mortality_rate_6, rep(0.1, 6)),
    tar_target(sd_natural_mortality_rate_6, rep(0.01, 6)),
    tar_target(mu_natural_mortality_rate_8, rep(0.1, 8)),
    tar_target(sd_natural_mortality_rate_8, rep(0.01, 8))
  ),
  # Define shared priors -------------------------------------------------------
  list(
    tar_target(mu_initial_loss_rate, 0.1),
    tar_target(sd_initial_loss_rate, 0.01),
    tar_target(mu_ongoing_loss_rate, 0.02),
    tar_target(sd_ongoing_loss_rate, 0.001),
    tar_target(mu_dispersion, 1.0),
    tar_target(sd_dispersion, 0.5)
  ),
  # Define tolerance arguments -------------------------------------------------
  list(
    tar_target(tolerance_expected, 1e-12),
    tar_target(tolerance_fishing, 1e-12)
  ),
  # Define CmdStanR arguments --------------------------------------------------
  list(
    tar_target(chains, 1L), # TODO: Update to 3 for analysis
    tar_target(step_size, 0.01),
    tar_target(adapt_delta, 0.95),
    tar_target(iter_warmup, 250),
    tar_target(iter_sampling, 1000),
    tar_target(max_treedepth, 10),
    tar_target(use_reduce_sum, TRUE),
    tar_target(threads_per_chain, parallel::detectCores() / (2 * chains)),
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
  # Assemble tag data no recovery transition -----------------------------------
  list(
    tar_target(
      tag_data_no_recovery_transition, # Caution: filtering tags recovered only
      tag_data %>%                     # Lose about 16 % of recovered
        dplyr::filter(                 # And no corresponding loss of released
          ((size_released %in% c(400:549, NA)) &
             (size_recovered %in% c(400:549, NA))) |
            ((size_released %in% c(550:800, NA)) &
               (size_recovered %in% c(550:800, NA)))
        )
    )
  ),
  # Assemble tag data 3 regions ------------------------------------------------
  list(
    tar_target(
      tag_data_3,
      tag_data %>%
        dplyr::mutate(year_released = lubridate::year(date_released)) %>%
        dplyr::mutate(year_recovered = lubridate::year(date_recovered)) %>%
        dplyr::mutate(region_released = regions_8_to_3(region_released)) %>%
        dplyr::mutate(region_recovered = regions_8_to_3(region_recovered)) %>%
        dplyr::select(
          tag_id,
          year_released,
          size_released,
          region_released,
          source,
          year_recovered,
          size_recovered,
          region_recovered,
          days_liberty,
          tag_distance
        )
    )
  ),
  # Assemble fishing rate priors -----------------------------------------------
  list(
    # Regions 3
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
    # Regions 6
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
    # Regions 8
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
    # CV
    tar_target(cv_fishing_rate, 0.1)
  ),
  # Plot map regions 6 ---------------------------------------------------------
  list(
    tar_target(
      map_regions_6,
      plot_map(
        regions = sf_omregions,
        plot_name = "map-regions-6",
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
  # Plot map regions 8 ---------------------------------------------------------
  list(
    tar_target(
      map_regions_8,
      plot_map(
        regions = sf_regions,
        plot_name = "map-regions-8",
        size_short = 1.75,
        size_line = 0.25,
        size_text = 5,
        color_land = "white",
        color_ocean = "grey96",
        color_region = "grey30",
        fill_land = "white",
        fill_ocean = "grey95",
        fill_region = "grey85",
        nudge_x = c(0, 0, 0, 0, 0, 0, 0, -1),
        nudge_y = c(0, 0, 0, -1, 0.5, 0.25, 0, 0.25),
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
      mmmstan_regions_3_mean, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 6 mean ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_6_mean, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_6, #
        list_sizes = list_sizes_1, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_omregion_released, #
        colname_region_recovered = colname_omregion_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_6, #
        sd_movement_step_diag = sd_movement_step_diag_6, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_6, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_6, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_6, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_6, #
        sd_reporting_rate = sd_reporting_rate_6, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 8 mean ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_8_mean, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_8, #
        list_sizes = list_sizes_1, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = matrix(c(5, 7, 7, 5), nrow = 2, byrow = TRUE), #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_8, #
        sd_movement_step_diag = sd_movement_step_diag_8, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_8, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_8, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_8, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_8, #
        sd_reporting_rate = sd_reporting_rate_8, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 size ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_size, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_2, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = mu_selectivity_3, #
        cv_selectivity = cv_selectivity, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 6 size ---------------------------------------------------------
  list(
    tar_target(
      mmmstan_regions_6_size, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_6, #
        list_sizes = list_sizes_2, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_omregion_released, #
        colname_region_recovered = colname_omregion_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_6, #
        sd_movement_step_diag = sd_movement_step_diag_6, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_6, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = mu_selectivity_6, #
        cv_selectivity = cv_selectivity, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_6, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_6, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_6, #
        sd_reporting_rate = sd_reporting_rate_6, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 mean block 1979-1995 -----------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean_block_1979_1995, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = 1979,
        year_end = 1995,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3[1:17,], #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 mean block 1995-2010 -----------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean_block_1995_2010, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = 1995,
        year_end = 2010,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3[17:32,], #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 mean block 2007-2017 -----------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean_block_2007_2017, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = 2007,
        year_end = 2017,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3[29:39,], #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit region 3 mean 3x cv_fishing_rate ---------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean_3x_cv_fishing_rate, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate * 3, #
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit region 3 mean 3x sd_reporting_rate -------------------------------------
  list(
    tar_target(
      mmmstan_regions_3_mean_3x_sd_reporting_rate, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_1, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = NULL, #
        cv_selectivity = NULL, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3 * 3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 size no duration constraint ----------------------------------
  list(
    tar_target(
      mmmstan_regions_3_size_no_duration_constraint, #
      mmmstan::mmmstan(
        tag_data = tag_data,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_2, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = 42, # (2017 - 1979 + 1) x 4
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = mu_selectivity_3, #
        cv_selectivity = cv_selectivity, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),
  # Fit regions 3 size no recovery transition ----------------------------------
  list(
    tar_target(
      mmmstan_regions_3_size_no_recovery_transition, #
      mmmstan::mmmstan(
        tag_data = tag_data_no_recovery_transition,
        # Tag arguments
        list_regions = list_regions_3, #
        list_sizes = list_sizes_2, #
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_duration_max = step_duration_max,
        colname_date_released = colname_date_released,
        colname_date_recovered = colname_date_recovered,
        colname_region_released = colname_region_released, #
        colname_region_recovered = colname_region_recovered, #
        colname_size_released = colname_size_released,
        # Movement index
        movement_pattern = 2L, # See: ?mmmstan::create_movement_index()
        movement_allow = NULL, #
        movement_disallow = NULL, #
        # Movement step mean priors
        mu_movement_step_diag = mu_movement_step_diag_3, #
        sd_movement_step_diag = sd_movement_step_diag_3, #
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate_3, #
        cv_fishing_rate = cv_fishing_rate,
        # Selectivity priors
        mu_selectivity = mu_selectivity_3, #
        cv_selectivity = cv_selectivity, #
        # Fishing weight priors
        mu_fishing_weight = NULL, # Not implemented
        sd_fishing_weight = NULL, # Not implemented
        # Natural mortality rate priors
        mu_natural_mortality_rate = mu_natural_mortality_rate_3, #
        sd_natural_mortality_rate = sd_natural_mortality_rate_3, #
        # Fractional (per tag) reporting rate priors
        mu_reporting_rate = mu_reporting_rate_3, #
        sd_reporting_rate = sd_reporting_rate_3, #
        # Fractional (per tag) initial loss rate priors
        mu_initial_loss_rate = mu_initial_loss_rate,
        sd_initial_loss_rate = sd_initial_loss_rate,
        # Instantaneous ongoing loss rate priors
        mu_ongoing_loss_rate = mu_ongoing_loss_rate,
        sd_ongoing_loss_rate = sd_ongoing_loss_rate,
        # Dispersion priors
        mu_dispersion = mu_dispersion,
        sd_dispersion = sd_dispersion,
        # Tolerance values
        tolerance_expected = tolerance_expected,
        tolerance_fishing = tolerance_fishing,
        # CmdStanR
        data = NULL,
        chains = chains,
        step_size = step_size,
        adapt_delta = adapt_delta,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        max_treedepth = max_treedepth,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        refresh = refresh
      )
    )
  ),

  # # Compute abundance exchange -------------------------------------------------
  # list(
  #   tar_target(
  #     abundance_exchange,
  #     create_abundance_exchange(
  #       abundance,
  #       mmmstan_regions_3_mean$summary$movement_rate
  #     )
  #   ),
  #   list()
  # ),

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
  # Plot heat regions 6 mean ---------------------------------------------------
  list(
    tar_target(
      heat_regions_6_mean,
      plot_heat(
        data = mmmstan_regions_6_mean$summary$movement_rate,
        plot_name = "heat-regions-6-mean",
        regions = toupper(names(list_regions_6)),
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
  # Plot bar regions 3 size ----------------------------------------------------
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
  # Plot bar regions 6 size ----------------------------------------------------
  list(
    tar_target(
      bar_regions_6_size,
      plot_cols(
        data = mmmstan_regions_6_size$summary$movement_rate,
        plot_name = "bar-regions-6-size",
        regions = toupper(names(list_regions_6)),
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
  # Plot heat regions 3 mean block 1979-1995 -----------------------------------
  list(
    tar_target(
      heat_regions_3_mean_block_1979_1995,
      plot_heat(
        data = mmmstan_regions_3_mean_block_1979_1995$summary$movement_rate,
        plot_name = "heat-regions-3-mean-block-1979-1995",
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
  # Plot heat regions 3 mean block 1995-2010 -----------------------------------
  list(
    tar_target(
      heat_regions_3_mean_block_1995_2010,
      plot_heat(
        data = mmmstan_regions_3_mean_block_1995_2010$summary$movement_rate,
        plot_name = "heat-regions-3-mean-block-1995-2010",
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
  # Plot heat regions 3 mean block 2007-2017 -----------------------------------
  list(
    tar_target(
      heat_regions_3_mean_block_2007_2017,
      plot_heat(
        data = mmmstan_regions_3_mean_block_2007_2017$summary$movement_rate,
        plot_name = "heat-regions-3-mean-block-2007-2017",
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
  # Plot heat regions 3 mean 3x cv_fishing_rate --------------------------------
  list(
    tar_target(
      heat_regions_3_mean_3x_cv_fishing_rate,
      plot_heat(
        data = mmmstan_regions_3_mean_3x_cv_fishing_rate$summary$movement_rate,
        plot_name = "heat-regions-3-mean-3x-cv-fishing-rate",
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
  # Plot heat regions 3 mean 3x sd_reporting_rate ------------------------------
  list(
    tar_target(
      heat_regions_3_mean_3x_sd_reporting_rate,
      plot_heat(
        data = mmmstan_regions_3_mean_3x_sd_reporting_rate$summary$movement_rate,
        plot_name = "heat-regions-3-mean-3x-sd-reporting-rate",
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
  # Plot bar regions 3 size no duration constraint -----------------------------
  list(
    tar_target(
      bar_regions_3_size_no_duration_constraint,
      plot_cols(
        data = mmmstan_regions_3_size_no_duration_constraint$summary$movement_rate,
        plot_name = "bar-regions-3-size-no-duration-constraint",
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
  # Plot bar regions 3 size no recovery transition -----------------------------
  list(
    tar_target(
      bar_regions_3_size_no_recovery_transition,
      plot_cols(
        data = mmmstan_regions_3_size_no_recovery_transition$summary$movement_rate,
        plot_name = "bar-regions-3-size-no-recovery-transition",
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
  # Plot bar regions 3 fishing priors posteriors -------------------------------
  list(
    tar_target(
      bar_regions_3_fishing_priors_posteriors,
      plot_fishing_priors_posteriors(
        plot_name = "bar-regions-3-fishing-priors-posteriors",
        data_prior_means = mu_fishing_rate_3,
        data_prior_cv = cv_fishing_rate,
        data_posteriors = mmmstan_regions_3_mean$summary$fishing_rate,
        year_start = year_start,
        year_xmin = year_start,
        year_xmax = 2020,
        regions = toupper(names(list_regions_3)),
        bar_width = 0.75,
        position_dodge = 0.8,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 mortality priors posteriors -----------------------------
  list(
    tar_target(
      bar_regions_3_mortality_priors_posteriors,
      plot_mortality_priors_posteriors(
        plot_name = "bar-regions-3-mortality-priors-posteriors",
        data_prior_means = mu_natural_mortality_rate_3,
        data_prior_sd = sd_natural_mortality_rate_3,
        data_posteriors = mmmstan_regions_3_mean$summary$natural_mortality_rate,
        regions = toupper(names(list_regions_3)),
        bar_width = 0.75,
        position_dodge = 0.8,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 reporting priors posteriors -----------------------------
  list(
    tar_target(
      bar_regions_3_reporting_priors_posteriors,
      plot_reporting_priors_posteriors(
        plot_name = "bar-regions-3-reporting-priors-posteriors",
        data_prior_means = mu_reporting_rate_3,
        data_prior_sd = sd_reporting_rate_3,
        data_posteriors = mmmstan_regions_3_mean$summary$reporting_rate,
        regions = toupper(names(list_regions_3)),
        bar_width = 0.75,
        position_dodge = 0.8,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 released by year ----------------------------------------
  list(
    tar_target(
      bar_regions_3_released_by_year,
      plot_released_by_year(
        plot_name = "bar-regions-3-released-by-year",
        data = tag_data_3,
        size_range = 400:800,
        year_start = year_start,
        year_xmin = year_start,
        year_xmax = 2020,
        regions = toupper(names(list_regions_3)),
        bar_width = 0.75,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 released by size ----------------------------------------
  list(
    tar_target(
      bar_regions_3_released_by_size,
      plot_released_by_size(
        plot_name = "bar-regions-3-released-by-size",
        data = tag_data_3,
        regions = toupper(names(list_regions_3)),
        size_range = 200:1000,
        year_range = 1979:2017,
        binwidth = 10,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 recovered by year ---------------------------------------
  list(
    tar_target(
      bar_regions_3_recovered_by_year,
      plot_recovered_by_year(
        plot_name = "bar-regions-3-recovered-by-year",
        data = tag_data_3,
        size_range = 400:800, # Released size
        year_range = 1979:2017,
        year_xmin = 1979,
        year_xmax = 2020,
        regions = toupper(names(list_regions_3)),
        bar_width = 0.75,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot bar regions 3 duration at liberty -------------------------------------
  list(
    tar_target(
      bar_regions_3_duration_at_liberty, # Based on days_liberty
      plot_duration_at_liberty(          # Model uses duration in steps
        plot_name = "bar-regions-3-duration-at-liberty",
        data = tag_data_3,
        size_range = 400:800, # Released size
        year_range = 1979:2017,
        regions = toupper(names(list_regions_3)),
        binwidth = 30,
        width = 190,
        height = 100,
        dpi = 300,
        file_type = ".png"
      ),
      format = "file"
    )
  ),
  # Plot map regions 3 released ------------------------------------------------

  # Copied from sablefish-data/ to preserve data privacy

  # Plot map regions 3 recovered -----------------------------------------------

  # Copied from sablefish-data/ to preserve data privacy



  # # Plot abundance exchange ----------------------------------------------------
  # list(
  #   tar_target(
  #     bar_abundance_exchange,
  #     plot_exchange(
  #       data = abundance_exchange,
  #       plot_name = "bar-abundance-exchange",
  #       size_hline = 0.5,
  #       size_error = 0.5,
  #       width = 190,
  #       height = 140,
  #       dpi = figure_dpi,
  #       file_type = figure_ext
  #     ),
  #     format = "file"
  #   )
  # ),
  list()
)
