library(targets)

# Set options
options(tidyverse.quiet = TRUE)
tar_option_set(packages = "sablefishmovement", imports = "sablefishmovement")

# List target objects
list(
  # Watch data -----------------------------------------------------------------
  list(
    tar_target(
      watch_harvest_rates,
      "data/harvest_rates.rda",
      format = "file"
    ),
    tar_target(
      watch_numbers_at_length,
      "data/numbers_at_length.rda",
      format = "file"
    ),
    tar_target(
      watch_tags_recovered,
      "data/tags_recovered.rda",
      format = "file"
    ),
    tar_target(
      watch_tags_released,
      "data/tags_released.rda",
      format = "file"
    )
  ),
  # Read data ------------------------------------------------------------------
  list(
    tar_target(harvest_rates, read_from_path(watch_harvest_rates)),
    tar_target(numbers_at_length, read_from_path(watch_numbers_at_length)),
    tar_target(tags_recovered, read_from_path(watch_tags_recovered)),
    tar_target(tags_released, read_from_path(watch_tags_released))
  ),
  # Define shared fit aruments -------------------------------------------------
  list(
    # CmdStanR
    tar_target(chains, 1L),
    tar_target(nuts_step_size, 0.01),
    tar_target(iter_warmup, 250),
    tar_target(iter_sampling, 750),
    tar_target(use_reduce_sum, TRUE),
    tar_target(threads_per_chain, 16L),
    # Model
    tar_target(released_time_unit, "quarter"),
    tar_target(released_time_max, 152), # 148
    tar_target(liberty_time_max, 160), # 12
    tar_target(released_date_start, "1979-01-01"),
    tar_target(released_date_end,  "2016-12-31"), # "2015-12-31"
    tar_target(tag_loss_rate_initial, 0.1),
    tar_target(tag_loss_rate_ongoing, 0.02),
    tar_target(natural_mortality_rate, 0.1),
    tar_target(movement_rate_fudge, 1e-12),
    tar_target(predicted_tags_fudge, 1e-12),
    tar_target(h_prior_sd, 0.01),
    tar_target(phi_prior_mean, 0.6),
    tar_target(phi_prior_sd, 0.2),
    # Spatial
    tar_target(
      area_list_region,
      list(ak = 1, bc = 2, cc = 3)
    ),
    tar_target(
      area_list_subregion,
      list(bs=1, ai=2, wg=3, cg=4, eg=5, se=6, nb=7, sb=8, nc=9, sc=10)
    ),
    tar_target(
      area_list_omregion,
      list(wak = 1, eak = 2, nbc = 3, sbc = 4, ncc = 5, scc = 6)
    )
  ),
  # Fit region-average-pooled --------------------------------------------------
  list(
    tar_target(
      fit_region_average_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled,
      write_data(fit_region_average_pooled)
    )
  ),
  # Fit region-average-length --------------------------------------------------
  list(
    tar_target(
      fit_region_average_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_length,
      write_data(fit_region_average_length)
    )
  ),
  # Fit region-season-pooled --------------------------------------------------
  list(
    tar_target(
      fit_region_season_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "season",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 4,
        harvest_time_max = 32,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_season_pooled,
      write_data(fit_region_season_pooled)
    )
  ),
  # Fit region-season-length --------------------------------------------------
  list(
    tar_target(
      fit_region_season_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "season",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 4,
        harvest_time_max = 32,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_season_length,
      write_data(fit_region_season_length)
    )
  ),
  # Fit region-year-pooled --------------------------------------------------
  list(
    tar_target(
      fit_region_year_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "year",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 40,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = 0.01,
        sigma_prior_sd = 0.005
      )$parameters
    ),
    tar_target(
      write_region_year_pooled,
      write_data(fit_region_year_pooled)
    )
  ),
  # Fit region-year-length --------------------------------------------------
  list(
    tar_target(
      fit_region_year_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "year",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 40,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = 0.01,
        sigma_prior_sd = 0.005
      )$parameters
    ),
    tar_target(
      write_region_year_length,
      write_data(fit_region_year_length)
    )
  ),
   # Fit subregion-average-pooled --------------------------------------------------
  list(
    tar_target(
      fit_subregion_average_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd =numeric(0)
      )$parameters
    ),
    tar_target(
      write_subregion_average_pooled,
      write_data(fit_subregion_average_pooled)
    )
  ),
  # Fit subregion-average-length --------------------------------------------------
  list(
    tar_target(
      fit_subregion_average_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "average",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_subregion_average_length,
      write_data(fit_subregion_average_length)
    )
  ),
  # Fit subregion-season-pooled --------------------------------------------------
  list(
    tar_target(
      fit_subregion_season_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "season",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 4,
        harvest_time_max = 32,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_subregion_season_pooled,
      write_data(fit_subregion_season_pooled)
    )
  ),
  # Fit subregion-season-length --------------------------------------------------
  list(
    tar_target(
      fit_subregion_season_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "season",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 4,
        harvest_time_max = 32,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_subregion_season_length,
      write_data(fit_subregion_season_length)
    )
  ),
  # Fit subregion-year-pooled --------------------------------------------------
  list(
    tar_target(
      fit_subregion_year_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "year",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 40,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = 0.01,
        sigma_prior_sd = 0.005
      )$parameters
    ),
    tar_target(
      write_subregion_year_pooled,
      write_data(fit_subregion_year_pooled)
    )
  ),
  # Fit subregion-year-length --------------------------------------------------
  list(
    tar_target(
      fit_subregion_year_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "subregion",
        temporal_name = "year",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_subregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_subregion",
        colname_id = "tag_id",
        area_list = area_list_subregion,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 40,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(
          c(rep(.4, 6), rep(.5, 2), rep(.3, 2)),
          dim = c(1, 10)
        ),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = 0.01,
        sigma_prior_sd = 0.005
      )$parameters
    ),
    tar_target(
      write_subregion_year_length,
      write_data(fit_subregion_year_length)
    )
  ),
  # Fit omregion-average-pooled --------------------------------------------------
  list(
    tar_target(
      fit_omregion_average_pooled,
      fit_movement_model(
        # Identifiers
        spatial_name = "omregion",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_omregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_omregion",
        colname_id = "tag_id",
        area_list = area_list_omregion,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .4, .5, .5, .3, .3), c(1, 6)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_omregion_average_pooled,
      write_data(fit_omregion_average_pooled)
    )
  ),
  # Fit omregion-average-length --------------------------------------------------
  list(
    tar_target(
      fit_omregion_average_length,
      fit_movement_model(
        # Identifiers
        spatial_name = "omregion",
        temporal_name = "average",
        grouping_name = "length",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_omregion",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_omregion",
        colname_id = "tag_id",
        area_list = area_list_omregion,
        group_list = list(small = 400:549, large = 550:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .4, .5, .5, .3, .3), c(1, 6)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_omregion_average_length,
      write_data(fit_omregion_average_length)
    )
  ),
  # Fit region-average-pooled-h-prior-sd-001 -----------------------------------
  list(
    tar_target(
      fit_region_average_pooled_h_prior_sd_001,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = 0.001,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_h_prior_sd_001,
      write_data(fit_region_average_pooled_h_prior_sd_001)
    )
  ),
  # Fit region-average-pooled-h-prior-sd-003 -----------------------------------
  list(
    tar_target(
      fit_region_average_pooled_h_prior_sd_003,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = 0.003,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_h_prior_sd_003,
      write_data(fit_region_average_pooled_h_prior_sd_003)
    )
  ),
  # Fit region-average-pooled-h-prior-sd-030 -----------------------------------
  list(
    tar_target(
      fit_region_average_pooled_h_prior_sd_030,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = 0.030,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_h_prior_sd_030,
      write_data(fit_region_average_pooled_h_prior_sd_030)
    )
  ),
  # Fit region-average-pooled-h-prior-sd-100 -----------------------------------
  list(
    tar_target(
      fit_region_average_pooled_h_prior_sd_100,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = 0.100,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_h_prior_sd_100,
      write_data(fit_region_average_pooled_h_prior_sd_100)
    )
  ),
  # Fit region-average-pooled-w-decr-ak-33-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_decr_ak_33_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4 * .67, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_decr_ak_33_pct,
      write_data(fit_region_average_pooled_w_decr_ak_33_pct)
    )
  ),
  # Fit region-average-pooled-w-decr-bc-33-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_decr_bc_33_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5 * .67, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_decr_bc_33_pct,
      write_data(fit_region_average_pooled_w_decr_bc_33_pct)
    )
  ),
  # Fit region-average-pooled-w-decr-cc-33-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_decr_cc_33_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3 * .67), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_decr_cc_33_pct,
      write_data(fit_region_average_pooled_w_decr_cc_33_pct)
    )
  ),
  # Fit region-average-pooled-w-incr-ak-50-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_incr_ak_50_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4 * 1.5, .5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_incr_ak_50_pct,
      write_data(fit_region_average_pooled_w_incr_ak_50_pct)
    )
  ),
  # Fit region-average-pooled-w-incr-bc-50-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_incr_bc_50_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5 * 1.5, .3), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_incr_bc_50_pct,
      write_data(fit_region_average_pooled_w_incr_bc_50_pct)
    )
  ),
  # Fit region-average-pooled-w-incr-cc-50-pct ---------------------------------
  list(
    tar_target(
      fit_region_average_pooled_w_incr_cc_50_pct,
      fit_movement_model(
        # Identifiers
        spatial_name = "region",
        temporal_name = "average",
        grouping_name = "pooled",
        # CmdStanR arguments
        chains = chains,
        nuts_step_size = nuts_step_size,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        use_reduce_sum = use_reduce_sum,
        threads_per_chain = threads_per_chain,
        # Data
        tags_released = tags_released,
        tags_recovered = tags_recovered,
        harvest_rates = harvest_rates,
        # Data arguments
        released_time_unit = released_time_unit,
        released_time_max = released_time_max,
        liberty_time_max = liberty_time_max,
        colname_released_date = "released_date",
        colname_released_area = "released_region",
        colname_group = "released_length",
        colname_recovered_date = "recovered_date",
        colname_recovered_area = "recovered_region",
        colname_id = "tag_id",
        area_list = area_list_region,
        group_list = list(pooled = 400:800),
        released_date_start = released_date_start,
        released_date_end = released_date_end,
        # Data list arguments
        movement_time_max = 1,
        harvest_time_max = 40,
        reporting_time_max = 1,
        tag_loss_rate_initial = tag_loss_rate_initial,
        tag_loss_rate_ongoing = tag_loss_rate_ongoing,
        natural_mortality_rate = natural_mortality_rate,
        reporting_rates = array(c(.4, .5, .3 * 1.5), c(1, 3)),
        movement_rate_fudge = movement_rate_fudge,
        predicted_tags_fudge = predicted_tags_fudge,
        h_prior_sd = h_prior_sd,
        phi_prior_mean = phi_prior_mean,
        phi_prior_sd = phi_prior_sd,
        sigma_prior_mean = numeric(0),
        sigma_prior_sd = numeric(0)
      )$parameters
    ),
    tar_target(
      write_region_average_pooled_w_incr_cc_50_pct,
      write_data(fit_region_average_pooled_w_incr_cc_50_pct)
    )
  ),
  # Plot
  list(
    tar_target(
      plot_pres_heat_region_average_pooled,
      pres_heat_pooled(
        data = fit_region_average_pooled$p,
        name = "pres-heat-region-average-pooled",
        movement_time = 1,
        released_group = 1,
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = -5,
        margin_y = -6,
        font_size_mean = 3,
        font_nudge_mean = 0.15,
        font_size_sd = 2,
        font_nudge_sd = 0.15,
        legend_name = "Movement rate",
        width = 3.5,
        height = 3.5
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_heat_subregion_average_pooled,
      pres_heat_pooled(
        data = fit_subregion_average_pooled$p,
        name = "pres-heat-subregion-average-pooled",
        movement_time = 1,
        released_group = 1,
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = 0,
        margin_y = 0,
        font_size_mean = 3,
        font_nudge_mean = 0.15,
        font_size_sd = 2,
        font_nudge_sd = 0.15,
        legend_name = "Movement rate",
        width = 4,
        height = 4
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_heat_omregion_average_pooled,
      pres_heat_pooled(
        data = fit_omregion_average_pooled$p,
        name = "pres-heat-omregion-average-pooled",
        movement_time = 1,
        released_group = 1,
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = 0,
        margin_y = 0,
        font_size_mean = 3,
        font_nudge_mean = 0.15,
        font_size_sd = 2,
        font_nudge_sd = 0.15,
        legend_name = "Movement rate",
        width = 3.5,
        height = 3.5
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_heat_region_average_length,
      pres_heat_length(
        data = fit_region_average_length$p,
        name = "pres-heat-region-average-length",
        movement_time = 1,
        released_group = c(1, 2),
        xlab = NULL,
        ylab = NULL,
        xtext = TRUE,
        ytext = TRUE,
        margin_x = -5,
        margin_y = -6,
        font_size_mean = 3,
        font_nudge_mean = 0.15,
        font_size_sd = 2,
        font_nudge_sd = 0.15,
        legend_name = "Movement rate",
        width = 5,
        height = 3
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_point_region_year_pooled,
      pres_point_movement(
        data = fit_region_year_pooled$p,
        name = "pres-point-region-year-pooled",
        released_group = 1,
        point_size = 0.5,
        xlab = NULL,
        ylab = NULL,
        xlim = c(1977, 2020),
        ylim = NULL,
        width = 5,
        height = 4
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_bar_region_quarter_pooled,
      pres_bar_quarter(
        data = fit_region_season_pooled$p,
        name = "pres-bar-region-season-pooled",
        released_group = 1,
        xlab = "Quarter",
        ylab = "Movement rate",
        ylim = c(0.6, 1),
        xtext = TRUE,
        ytext = TRUE,
        width = 5,
        height = 3,
        legend_name = "Region"
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_bar_region_quarter_length,
      pres_bar_quarter(
        data = fit_region_season_length$p,
        name = "pres-bar-region-season-length",
        released_group = 1:2,
        xlab = "Quarter",
        ylab = "Movement rate",
        ylim = c(0, 1),
        xtext = TRUE,
        ytext = TRUE,
        width = 5,
        height = 6,
        legend_name = "Region"
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_harvest_region_average_pooled,
      pres_harvest(
        data = fit_region_average_pooled$h,
        name = "pres-harvest-region-average-pooled",
        harvest_group = 1,
        xlab = "Year",
        ylab = "Harvest rate",
        xlim = c(1979, 2020),
        ylim = c(0, 0.2),
        point_size = 0.8,
        width = 5,
        height = 4
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_harvest_region_average_length,
      pres_harvest_length(
        data = fit_region_average_length$h,
        name = "pres-harvest-region-average-length",
        harvest_group = c(1, 2),
        xlab = "Year",
        ylab = "Harvest rate",
        xlim = c(1979, 2020),
        ylim = c(0, 0.2),
        point_size = 0.8,
        width = 5,
        height = 4
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_harvest_subregion_average_pooled,
      pres_harvest(
        data = fit_subregion_average_pooled$h,
        name = "pres-harvest-subregion-average-pooled",
        harvest_group = 1,
        xlab = "Year",
        ylab = "Harvest rate",
        xlim = c(1979, 2020),
        ylim = c(0, 0.25),
        point_size = 0.8,
        width = 5,
        height = 6
      ),
      format = "file"
    ),
    tar_target(
      plot_pres_abundance_exchange,
      pres_abundance_exchange(
        data = fit_region_year_length$p,
        numbers = numbers_at_length,
        name = "pres-abundance-exchange-region",
        years = 1979:2016,
        xlim = c(1979, 2016),
        ylim = c(-2e7, 1e7),
        width = 5,
        height = 4
      ),
      format = "file"
    )
  ),
  list()
)
