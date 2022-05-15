library(targets)
source("R/functions.R")
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
    tar_target(figure_type, ".png")
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
      list_regions,
      list(ai = 1, bs = 2, wg = 3, cg = 4, eg = 5, se = 6, bc = 7, cc = 8)
    ),
    tar_target(
      list_omregions,
      list(wak = 1, eak = 2, nbc = 3, sbc = 4, ncc = 5, scc = 6)
    ),
    list()
  ),
  # Define sizes ---------------------------------------------------------------
  list(
    tar_target(list_sizes, list(sml = 400:800)),
    tar_target(list_sizes_small_large, list(s = 400:549, l = 550:800)),
    list()
  ),
  # Define movement ------------------------------------------------------------
  list(
    tar_target(movement_pattern, 2L),
    tar_target(movement_allow, matrix(c(5, 7, 7, 5), nrow = 2, byrow = TRUE)),
    tar_target(movement_disallow, NULL),
    list()
  ),
  # Define model step ----------------------------------------------------------
  list(
    tar_target(step_interval, "quarter"),
    tar_target(step_liberty_max, 12L),
    list()
  ),
  # Define priors --------------------------------------------------------------
  list(
    tar_target(mu_reporting_rate, c(rep(0.4, 6), 0.5, 0.3)),
    tar_target(sd_reporting_rate, mu_reporting_rate * 0.1),
    list()
  ),
  # Define CmdStanR arguments --------------------------------------------------
  list(
    tar_target(chains, 1L),
    tar_target(use_reduce_sum, TRUE),
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
    # tar_target(
    #   watch_abundance,
    #   "data/abundance.rda",
    #   format = "file"
    # ),
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
    # tar_target(abundance, read_from_path(watch_abundance)),
    tar_target(sf_regions, read_from_path(watch_sf_regions)),
    tar_target(sf_omregions, read_from_path(watch_sf_omregions)),
    list()
  ),
  # Assemble fishing rate priors -----------------------------------------------
  list(
    tar_target(
      mu_fishing_rate,
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
    tar_target(
      mu_fishing_rate_omregion,
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
        file_type = figure_type
      ),
      format = "file"
    )
  ),
  # Fit mean -------------------------------------------------------------------
  list(
    tar_target(
      fit_mean,
      mmmstan::mmmstan(
        tag_data = tag_data,
        model_form = "mean",
        # Tag arguments
        list_regions = list_regions,
        list_sizes = list_sizes,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_liberty_max = step_liberty_max,
        # Movement index
        movement_pattern = movement_pattern,
        movement_allow = movement_allow,
        movement_disallow = movement_disallow,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate,
        cv_fishing_rate = 0.1,
        # Reporting rate priors
        mu_reporting_rate = mu_reporting_rate,
        sd_reporting_rate = sd_reporting_rate,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )$summary
    ),
    # Movement step mean priors
    tar_target(
      mu_movement_step_mean,
      fit_mean$movement_step  %>%
        dplyr::pull(.data$mean) %>%
        matrix(
          byrow = TRUE,
          nrow = length(list_regions),
          ncol = length(list_regions)
        )
    ),
    tar_target(
      sd_movement_step_mean,
      fit_mean$movement_step %>%
        dplyr::pull(.data$sd) %>%
        matrix(
          byrow = TRUE,
          nrow = length(list_regions),
          ncol = length(list_regions)
        )
    )
  ),
  # Fit time -------------------------------------------------------------------
  list(
    tar_target(
      fit_time,
      mmmstan::mmmstan(
        tag_data = tag_data,
        model_form = "time",
        # Tag arguments
        list_regions = list_regions,
        list_sizes = list_sizes,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_liberty_max = step_liberty_max,
        # Movement index
        movement_pattern = movement_pattern,
        movement_allow = movement_allow,
        movement_disallow = movement_disallow,
        # Movement step mean priors (from fit mean)
        mu_movement_step_mean = mu_movement_step_mean,
        sd_movement_step_mean = sd_movement_step_mean,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate,
        cv_fishing_rate = 0.1,
        # Reporting rate priors
        mu_reporting_rate = mu_reporting_rate,
        sd_reporting_rate = sd_reporting_rate,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )$summary
    )
  ),
  # Fit term -------------------------------------------------------------------
  list(
    tar_target(
      fit_term,
      mmmstan::mmmstan(
        tag_data = tag_data,
        model_form = "term",
        # Tag arguments
        list_regions = list_regions,
        list_sizes = list_sizes,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_liberty_max = step_liberty_max,
        # Movement index
        movement_pattern = movement_pattern,
        movement_allow = movement_allow,
        movement_disallow = movement_disallow,
        # Movement step mean priors (from fit mean)
        mu_movement_step_mean = mu_movement_step_mean,
        sd_movement_step_mean = sd_movement_step_mean,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate,
        cv_fishing_rate = 0.1,
        # Reporting rate priors
        mu_reporting_rate = mu_reporting_rate,
        sd_reporting_rate = sd_reporting_rate,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )$summary
    )
  ),
  # Fit size -------------------------------------------------------------------
  list(
    tar_target(
      fit_size,
      mmmstan::mmmstan(
        tag_data = tag_data,
        model_form = "size",
        # Tag arguments
        list_regions = list_regions,
        list_sizes = list_sizes_small_large,
        year_start = year_start,
        year_end = year_end,
        step_interval = step_interval,
        step_liberty_max = step_liberty_max,
        # Movement index
        movement_pattern = movement_pattern,
        movement_allow = movement_allow,
        movement_disallow = movement_disallow,
        # Movement step mean priors (from fit mean)
        mu_movement_step_mean = mu_movement_step_mean,
        sd_movement_step_mean = sd_movement_step_mean,
        # Fishing rate priors
        mu_fishing_rate = mu_fishing_rate,
        cv_fishing_rate = 0.1,
        # Reporting rate priors
        mu_reporting_rate = mu_reporting_rate,
        sd_reporting_rate = sd_reporting_rate,
        # Use reduce sum
        use_reduce_sum = use_reduce_sum,
        refresh = refresh
      )$summary
    )
  ),
  # Plot heat mean -------------------------------------------------------------
  list(
    tar_target(
      heat_mean,
      plot_heat(
        data = fit_mean$movement_mean,
        plot_name = "heat-mean",
        regions = toupper(names(list_regions)),
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
        file_type = figure_type
      ),
      format = "file"
    )
  ),
  # Plot bar time --------------------------------------------------------------
  list(
    tar_target(
      bar_time,
      plot_cols(
        data = fit_time$movement_time,
        plot_name = "bar-time",
        regions = toupper(names(list_regions)),
        xvar = "i",
        xlab = "Year",
        ylab = "Annual movement rate",
        x_text = as.character(seq(1980, 2020, 10)),
        x_breaks = seq(2, 42, 10),
        y_text = as.character(seq(0, 1, 0.25)),
        y_breaks = seq(0, 1, 0.25),
        x_angle = 30,
        hjust = 0.8,
        vjust = 1,
        size_title = 8,
        size_strip = 8,
        size_text = 6,
        size_error = 0.1,
        panel_spacing = 1,
        xmin = 0,
        xmax = 43,
        ymin = 0.0,
        ymax = 1.0,
        width = 190,
        height = 170,
        dpi = 600,
        file_type = figure_type
      ),
      format = "file"
    )
  ),
  # Plot bar term --------------------------------------------------------------
  list(
    tar_target(
      bar_term,
      plot_cols(
        data = fit_term$movement_term,
        plot_name = "bar-term",
        regions = toupper(names(list_regions)),
        xvar = "i",
        xlab = "Year",
        ylab = "Quarterly movement rate",
        x_text = paste0("Q", 1:4),
        x_breaks = 1:4,
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
        file_type = figure_type
      ),
      format = "file"
    )
  ),
  # Plot bar size --------------------------------------------------------------
  list(
    tar_target(
      bar_size,
      plot_cols(
        data = fit_size$movement_size,
        plot_name = "bar-size",
        regions = toupper(names(list_regions)),
        xvar = "d",
        xlab = "Year",
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
        file_type = figure_type
      )
    ),
    format = "file"
  ),
  list()
)


#   # Fit region-average-pooled --------------------------------------------------
#   list(
#     tar_target(
#       region_average_pooled,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled,
#       write_data(region_average_pooled, path = "fits")
#     )
#   ),
#   # Fit region-average-length --------------------------------------------------
#   list(
#     tar_target(
#       region_average_length,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "length",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(small = 400:549, large = 550:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_length,
#       write_data(region_average_length, path = "fits")
#     )
#   ),
#   # Fit region-season-pooled --------------------------------------------------
#   list(
#     tar_target(
#       region_season_pooled,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "season",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 4,
#         harvest_time_max = 32,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_season_pooled,
#       write_data(region_season_pooled, path = "fits")
#     )
#   ),
#   # Fit region-season-length --------------------------------------------------
#   list(
#     tar_target(
#       region_season_length,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "season",
#         grouping_name = "length",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(small = 400:549, large = 550:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 4,
#         harvest_time_max = 32,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_season_length,
#       write_data(region_season_length, path = "fits")
#     )
#   ),
#   # Fit region-year-pooled --------------------------------------------------
#   list(
#     tar_target(
#       region_year_pooled,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "year",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 40,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = 0.01,
#         sigma_prior_sd = 0.005,
#         # Cmdstanr::sample()
#         max_treedepth = 12,
#         adapt_delta = 0.95
#       )$summaries
#     ),
#     tar_target(
#       write_region_year_pooled,
#       write_data(region_year_pooled, path = "fits")
#     )
#   ),
#   # Fit region-year-length --------------------------------------------------
#   list(
#     tar_target(
#       region_year_length,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "year",
#         grouping_name = "length",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(small = 400:549, large = 550:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 40,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = 0.01,
#         sigma_prior_sd = 0.005
#       )$summaries
#     ),
#     tar_target(
#       write_region_year_length,
#       write_data(region_year_length, path = "fits")
#     )
#   ),
#   # Fit omregion-average-pooled --------------------------------------------------
#   list(
#     tar_target(
#       omregion_average_pooled,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "omregion",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_omregion",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_omregion",
#         colname_id = "tag_id",
#         area_list = area_list_omregion,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(.4, .4, .5, .5, .3, .3), c(1, 6)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_omregion_average_pooled,
#       write_data(omregion_average_pooled, path = "fits")
#     )
#   ),
#   # Fit omregion-average-length --------------------------------------------------
#   list(
#     tar_target(
#       omregion_average_length,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "omregion",
#         temporal_name = "average",
#         grouping_name = "length",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_omregion",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_omregion",
#         colname_id = "tag_id",
#         area_list = area_list_omregion,
#         group_list = list(small = 400:549, large = 550:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(.4, .4, .5, .5, .3, .3), c(1, 6)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_omregion_average_length,
#       write_data(omregion_average_length, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-h-prior-sd-001-pct -------------------------------
#   list(
#     tar_target(
#       region_average_pooled_h_prior_sd_001_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = 0.01,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_h_prior_sd_001_pct,
#       write_data(region_average_pooled_h_prior_sd_001_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-h-prior-sd-005-pct -------------------------------
#   list(
#     tar_target(
#       region_average_pooled_h_prior_sd_005_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = 0.05,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_h_prior_sd_005_pct,
#       write_data(region_average_pooled_h_prior_sd_005_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-h-prior-sd-010-pct -------------------------------
#   list(
#     tar_target(
#       region_average_pooled_h_prior_sd_010_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = 0.10,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_h_prior_sd_010_pct,
#       write_data(region_average_pooled_h_prior_sd_010_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-h-prior-sd-015-pct -------------------------------
#   list(
#     tar_target(
#       region_average_pooled_h_prior_sd_015_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = 0.15,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_h_prior_sd_015_pct,
#       write_data(region_average_pooled_h_prior_sd_015_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-h-prior-sd-020-pct -------------------------------
#   list(
#     tar_target(
#       region_average_pooled_h_prior_sd_020_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = 0.20,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_h_prior_sd_020_pct,
#       write_data(region_average_pooled_h_prior_sd_020_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-decr-ak-33-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_decr_ak_33_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6) * .67, .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_decr_ak_33_pct,
#       write_data(region_average_pooled_w_decr_ak_33_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-decr-bc-33-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_decr_bc_33_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5 * .67, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_decr_bc_33_pct,
#       write_data(region_average_pooled_w_decr_bc_33_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-decr-cc-33-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_decr_cc_33_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3 * .67), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_decr_cc_33_pct,
#       write_data(region_average_pooled_w_decr_cc_33_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-incr-ak-50-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_incr_ak_50_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6) * 1.5, .5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_incr_ak_50_pct,
#       write_data(region_average_pooled_w_incr_ak_50_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-incr-bc-50-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_incr_bc_50_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5 * 1.5, .3), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_incr_bc_50_pct,
#       write_data(region_average_pooled_w_incr_bc_50_pct, path = "fits")
#     )
#   ),
#   # Fit region-average-pooled-w-incr-cc-50-pct ---------------------------------
#   list(
#     tar_target(
#       region_average_pooled_w_incr_cc_50_pct,
#       fit_movement_model(
#         # Identifiers
#         spatial_name = "region",
#         temporal_name = "average",
#         grouping_name = "pooled",
#         # CmdStanR arguments
#         chains = chains,
#         nuts_step_size = nuts_step_size,
#         iter_warmup = iter_warmup,
#         iter_sampling = iter_sampling,
#         use_reduce_sum = use_reduce_sum,
#         threads_per_chain = threads_per_chain,
#         # Data
#         tags_released = tags_released,
#         tags_recovered = tags_recovered,
#         harvest_rates = harvest_rates,
#         # Data arguments
#         released_time_unit = released_time_unit,
#         released_time_max = released_time_max,
#         liberty_time_max = liberty_time_max,
#         colname_released_date = "released_date",
#         colname_released_area = "released_region",
#         colname_group = "released_length",
#         colname_recovered_date = "recovered_date",
#         colname_recovered_area = "recovered_region",
#         colname_id = "tag_id",
#         area_list = area_list_region,
#         group_list = list(pooled = 400:800),
#         released_date_start = released_date_start,
#         released_date_end = released_date_end,
#         # Data list arguments
#         movement_time_max = 1,
#         harvest_time_max = 40,
#         reporting_time_max = 1,
#         tag_loss_rate_initial = tag_loss_rate_initial,
#         tag_loss_rate_ongoing = tag_loss_rate_ongoing,
#         natural_mortality_rate = natural_mortality_rate,
#         reporting_rates = array(c(rep(.4, 6), .5, .3 * 1.5), c(1, 8)),
#         movement_rate_fudge = movement_rate_fudge,
#         predicted_tags_fudge = predicted_tags_fudge,
#         h_prior_sd_pct = h_prior_sd_pct,
#         phi_prior_mean = phi_prior_mean,
#         phi_prior_sd = phi_prior_sd,
#         sigma_prior_mean = numeric(0),
#         sigma_prior_sd = numeric(0)
#       )$summaries
#     ),
#     tar_target(
#       write_region_average_pooled_w_incr_cc_50_pct,
#       write_data(region_average_pooled_w_incr_cc_50_pct, path = "fits")
#     )
#   ),
#   # Plot -----------------------------------------------------------------------
#   list(
#     tar_target(
#       map_regions,
#       plot_map(
#         regions = sf_regions,
#         plot_name = "map-regions",
#         size_short = 1.75,
#         size_line = 0.25,
#         size_text = 5,
#         color_land = "white",
#         color_ocean = "grey96",
#         color_region = "grey30",
#         fill_land = "white",
#         fill_ocean = "grey95",
#         fill_region = "grey85",
#         xmin = 169,
#         ymin = 31,
#         xmax = 240.5,
#         ymax = 65.5,
#         width = 90,
#         height = 65,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       heat_region_average_pooled,
#       plot_heat(
#         data = region_average_pooled$p,
#         plot_name = "heat-region-average-pooled",
#         movement_time = 1,
#         released_group = 1,
#         size_text = 8,
#         xlab = NULL,
#         ylab = NULL,
#         xtext = TRUE,
#         ytext = TRUE,
#         margin_x = -1,
#         margin_y = -2,
#         font_size_mean = 3,
#         font_nudge_mean = 0.15,
#         font_size_sd = 2,
#         font_nudge_sd = 0.15,
#         legend_name = "Annual transition rates",
#         width = 90,
#         height = 98,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       heat_region_average_length,
#       plot_heat_length(
#         data = region_average_length$p,
#         plot_name = "heat-region-average-length",
#         movement_time = 1,
#         released_group = c(1, 2),
#         size_text = 8,
#         xlab = NULL,
#         ylab = NULL,
#         xtext = TRUE,
#         ytext = TRUE,
#         margin_x = -1,
#         margin_y = -2,
#         font_size_mean = 3,
#         font_nudge_mean = 0.15,
#         font_size_sd = 2,
#         font_nudge_sd = 0.15,
#         legend_name = "Annual transition rates",
#         width = 190,
#         height = 105,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       bar_sensitivity_reporting,
#       plot_bar_sensitivity_reporting(
#         study = region_average_pooled$p,
#         increase_ak = region_average_pooled_w_incr_ak_50_pct$p,
#         increase_bc = region_average_pooled_w_incr_bc_50_pct$p,
#         increase_cc = region_average_pooled_w_incr_cc_50_pct$p,
#         decrease_ak = region_average_pooled_w_decr_ak_33_pct$p,
#         decrease_bc = region_average_pooled_w_decr_bc_33_pct$p,
#         decrease_cc = region_average_pooled_w_decr_cc_33_pct$p,
#         plot_name = "bar-sensitivity-reporting",
#         size_text = 8,
#         legend_name = "Reporting rate",
#         width = 190,
#         height = 150,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       bar_sensitivity_harvest_priors,
#       plot_bar_sensitivity_harvest_priors(
#         study = region_average_pooled$p,
#         sd_001 = region_average_pooled_h_prior_sd_001_pct$p,
#         sd_005 = region_average_pooled_h_prior_sd_005_pct$p,
#         sd_010 = region_average_pooled_h_prior_sd_010_pct$p,
#         sd_015 = region_average_pooled_h_prior_sd_015_pct$p,
#         sd_020 = region_average_pooled_h_prior_sd_020_pct$p,
#         plot_name = "bar-sensitivity-harvest-priors",
#         size_text = 8,
#         legend_name = "Prior SD/Mean",
#         width = 190,
#         height = 80,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       bar_retention_region_season_pooled,
#       plot_bar_retention_region_season_pooled(
#         data = region_season_pooled$p,
#         plot_name = "bar-retention-region-season-pooled",
#         size_text = 8,
#         legend_name = "Quarter",
#         width = 190,
#         height = 80,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       bar_region_season_pooled,
#       plot_bar_region_season_pooled(
#         data = region_season_pooled$p,
#         plot_name = "bar-region-season-pooled",
#         size_text = 8,
#         legend_name = "Quarter",
#         width = 190,
#         height = 160,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       point_retention_region_year_pooled,
#       plot_point_retention_region_year_pooled(
#         data = region_year_pooled$p,
#         plot_name = "point-retention-region-year-pooled",
#         size_line = 0.2,
#         size_point = 0.75,
#         size_text = 8,
#         year_offset = 1978,
#         width = 90,
#         height = 150,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       point_region_year_pooled,
#       plot_point_region_year_pooled(
#         data = region_year_pooled$p,
#         plot_name = "point-region-year-pooled",
#         size_line = 0.2,
#         size_point = 0.25,
#         size_text = 8,
#         year_offset = 1978,
#         width = 190,
#         height = 160,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       bar_retention_region_season_length,
#       plot_bar_retention_region_season_length(
#         data = region_season_length$p,
#         plot_name = "bar-retention-region-season-length",
#         size_text = 8,
#         legend_name = "Quarter",
#         width = 190,
#         height = 150,
#         file_type = ".png"
#       ),
#       format = "file"
#     ),
#     tar_target(
#       point_retention_region_year_length,
#       plot_point_retention_region_year_length(
#         data = region_year_length$p,
#         plot_name = "point-retention-region-year-length",
#         size_line = 0.2,
#         size_point = 0.75,
#         size_text = 8,
#         year_offset = 1978,
#         width = 190,
#         height = 150,
#         file_type = ".png"
#       ),
#       format = "file"
#     )
#   ),
#
#
#
# # Old below here ---------------------------------------------------------------
#
#   # list(
#   #   tar_target(
#   #     plot_pres_heat_region_average_pooled,
#   #     pres_heat_pooled(
#   #       data = fit_region_average_pooled$p,
#   #       name = "pres-heat-region-average-pooled",
#   #       movement_time = 1,
#   #       released_group = 1,
#   #       xlab = NULL,
#   #       ylab = NULL,
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       margin_x = -5,
#   #       margin_y = -6,
#   #       font_size_mean = 3,
#   #       font_nudge_mean = 0.15,
#   #       font_size_sd = 2,
#   #       font_nudge_sd = 0.15,
#   #       legend_name = "Movement rate",
#   #       width = 3.5,
#   #       height = 3.5
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_heat_subregion_average_pooled,
#   #     pres_heat_pooled(
#   #       data = fit_subregion_average_pooled$p,
#   #       name = "pres-heat-subregion-average-pooled",
#   #       movement_time = 1,
#   #       released_group = 1,
#   #       xlab = NULL,
#   #       ylab = NULL,
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       margin_x = 0,
#   #       margin_y = 0,
#   #       font_size_mean = 3,
#   #       font_nudge_mean = 0.15,
#   #       font_size_sd = 2,
#   #       font_nudge_sd = 0.15,
#   #       legend_name = "Movement rate",
#   #       width = 4,
#   #       height = 4
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_heat_omregion_average_pooled,
#   #     pres_heat_pooled(
#   #       data = fit_omregion_average_pooled$p,
#   #       name = "pres-heat-omregion-average-pooled",
#   #       movement_time = 1,
#   #       released_group = 1,
#   #       xlab = NULL,
#   #       ylab = NULL,
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       margin_x = 0,
#   #       margin_y = 0,
#   #       font_size_mean = 3,
#   #       font_nudge_mean = 0.15,
#   #       font_size_sd = 2,
#   #       font_nudge_sd = 0.15,
#   #       legend_name = "Movement rate",
#   #       width = 3.5,
#   #       height = 3.5
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_heat_region_average_length,
#   #     pres_heat_length(
#   #       data = fit_region_average_length$p,
#   #       name = "pres-heat-region-average-length",
#   #       movement_time = 1,
#   #       released_group = c(1, 2),
#   #       xlab = NULL,
#   #       ylab = NULL,
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       margin_x = -5,
#   #       margin_y = -6,
#   #       font_size_mean = 3,
#   #       font_nudge_mean = 0.15,
#   #       font_size_sd = 2,
#   #       font_nudge_sd = 0.15,
#   #       legend_name = "Movement rate",
#   #       width = 5,
#   #       height = 3
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_point_region_year_pooled,
#   #     pres_point_movement(
#   #       data = fit_region_year_pooled$p,
#   #       name = "pres-point-region-year-pooled",
#   #       released_group = 1,
#   #       point_size = 0.5,
#   #       xlab = NULL,
#   #       ylab = NULL,
#   #       xlim = c(1977, 2020),
#   #       ylim = NULL,
#   #       width = 5,
#   #       height = 4
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_bar_region_quarter_pooled,
#   #     pres_bar_quarter(
#   #       data = fit_region_season_pooled$p,
#   #       name = "pres-bar-region-season-pooled",
#   #       released_group = 1,
#   #       xlab = "Quarter",
#   #       ylab = "Movement rate",
#   #       ylim = c(0.6, 1),
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       width = 5,
#   #       height = 3,
#   #       legend_name = "Region"
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_bar_region_quarter_length,
#   #     pres_bar_quarter(
#   #       data = fit_region_season_length$p,
#   #       name = "pres-bar-region-season-length",
#   #       released_group = 1:2,
#   #       xlab = "Quarter",
#   #       ylab = "Movement rate",
#   #       ylim = c(0, 1),
#   #       xtext = TRUE,
#   #       ytext = TRUE,
#   #       width = 5,
#   #       height = 6,
#   #       legend_name = "Region"
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_harvest_region_average_pooled,
#   #     pres_harvest(
#   #       data = fit_region_average_pooled$h,
#   #       name = "pres-harvest-region-average-pooled",
#   #       harvest_group = 1,
#   #       xlab = "Year",
#   #       ylab = "Harvest rate",
#   #       xlim = c(1979, 2020),
#   #       ylim = c(0, 0.2),
#   #       point_size = 0.8,
#   #       width = 5,
#   #       height = 4
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_harvest_region_average_length,
#   #     pres_harvest_length(
#   #       data = fit_region_average_length$h,
#   #       name = "pres-harvest-region-average-length",
#   #       harvest_group = c(1, 2),
#   #       xlab = "Year",
#   #       ylab = "Harvest rate",
#   #       xlim = c(1979, 2020),
#   #       ylim = c(0, 0.2),
#   #       point_size = 0.8,
#   #       width = 5,
#   #       height = 4
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_harvest_subregion_average_pooled,
#   #     pres_harvest(
#   #       data = fit_subregion_average_pooled$h,
#   #       name = "pres-harvest-subregion-average-pooled",
#   #       harvest_group = 1,
#   #       xlab = "Year",
#   #       ylab = "Harvest rate",
#   #       xlim = c(1979, 2020),
#   #       ylim = c(0, 0.25),
#   #       point_size = 0.8,
#   #       width = 5,
#   #       height = 6
#   #     ),
#   #     format = "file"
#   #   ),
#   #   tar_target(
#   #     plot_pres_abundance_exchange,
#   #     pres_abundance_exchange(
#   #       data = fit_region_year_length$p,
#   #       numbers = numbers_at_length,
#   #       name = "pres-abundance-exchange-region",
#   #       years = 1979:2016,
#   #       xlim = c(1979, 2016),
#   #       ylim = c(-2e7, 1e7),
#   #       width = 5,
#   #       height = 4
#   #     ),
#   #     format = "file"
#   #   )
#   # ),
#   list()
# )
