#' Fit Movement Model
#'
#' @param spatial_name  [character()]
#' @param temporal_name [character()]
#' @param grouping_name [character()]
#' @param chains [numeric()]
#' @param nuts_step_size [numeric()]
#' @param iter_warmup [numeric()]
#' @param iter_sampling [numeric()]
#' @param use_reduce_sum [logical()]
#' @param threads_per_chain [numeric()]
#' @param tags_released [data.frame()]
#' @param tags_recovered [data.frame()]
#' @param harvest_rates [data.frame()]
#' @param released_time_unit [character()]
#' @param released_time_max [numeric()]
#' @param liberty_time_max [numeric()]
#' @param colname_released_date [character()]
#' @param colname_released_area [character()]
#' @param colname_group [character()]
#' @param colname_recovered_date [character()]
#' @param colname_recovered_area [character()]
#' @param colname_id [character()]
#' @param area_list [list()]
#' @param group_list [list()]
#' @param released_date_start [character()]
#' @param released_date_end [character()]
#' @param movement_time_max [numeric()]
#' @param harvest_time_max [numeric()]
#' @param reporting_time_max [numeric()]
#' @param tag_loss_rate_initial [numeric()]
#' @param tag_loss_rate_ongoing [numeric()]
#' @param natural_mortality_rate [numeric()]
#' @param reporting_rates [numeric()]
#' @param movement_rate_fudge [numeric()]
#' @param predicted_tags_fudge [numeric()]
#' @param h_prior_mean [numeric()]
#' @param h_prior_sd [numeric()]
#' @param phi_prior_mean [numeric()]
#' @param phi_prior_sd [numeric()]
#' @param sigma_prior_mean [numeric()]
#' @param sigma_prior_sd [numeric()]
#' @param ... additional arguments to pass to cmdstanr \code{$sample()} method
#'
#' @importFrom rlang .data
#'
#' @return [mmmstan::mmmfit()] object
#' @export
#'
fit_movement_model <- function (spatial_name, # region, omregion
                                temporal_name, # average, season, year
                                grouping_name,  # pooled, length
                                # CmdStanR arguments
                                chains,
                                nuts_step_size,
                                iter_warmup,
                                iter_sampling,
                                use_reduce_sum,
                                threads_per_chain,
                                # Data
                                tags_released,
                                tags_recovered,
                                harvest_rates,
                                # Data arguments
                                released_time_unit,
                                released_time_max,
                                liberty_time_max,
                                colname_released_date,
                                colname_released_area,
                                colname_group,
                                colname_recovered_date,
                                colname_recovered_area,
                                colname_id,
                                area_list,
                                group_list,
                                released_date_start,
                                released_date_end,
                                # Data list arguments
                                movement_time_max,
                                harvest_time_max,
                                reporting_time_max,
                                tag_loss_rate_initial,
                                tag_loss_rate_ongoing,
                                natural_mortality_rate,
                                reporting_rates,
                                movement_rate_fudge = 1e-12,
                                predicted_tags_fudge = 1e-12,
                                h_prior_mean = NULL,
                                h_prior_sd,
                                phi_prior_mean,
                                phi_prior_sd,
                                sigma_prior_mean = numeric(0),
                                sigma_prior_sd = numeric(0),
                                # Additional arguments
                                ...) {

  # Check arguments ------------------------------------------------------------


  # Begin messages -------------------------------------------------------------

  cat(paste0("\n", spatial_name, "-", temporal_name, "-", grouping_name))
  cat("\n- preparing data")

  # Prepare data ---------------------------------------------------------------

  tags <- mmmstan::tag_arrays(
    released = tags_released,
    recovered = tags_recovered,
    released_time_unit = released_time_unit,
    released_time_max = released_time_max,
    liberty_time_max = liberty_time_max,
    liberty_days_min = 1,
    colname_released_date = colname_released_date,
    colname_released_area = colname_released_area,
    colname_group = colname_group,
    colname_recovered_date = colname_recovered_date,
    colname_recovered_area = colname_recovered_area,
    colname_id = colname_id,
    area_list = area_list,
    group_list = group_list,
    released_date_start = released_date_start,
    released_date_end = released_date_end
  )

  # Prepare constants ----------------------------------------------------------

  # G_harvest
  harvest_group_max <- length(group_list)
  # T_study
  if (released_time_max < liberty_time_max) {
    study_time_max <- liberty_time_max
  } else {
    study_time_max <- released_time_max + liberty_time_max - 1L
  }
  # T_year
  year_time_max <- unit_to_time(released_time_unit)

  # Prepare random walk argument -----------------------------------------------

  if (temporal_name == "year") {
    random_walk <- 1L
  } else {
    random_walk <- 0L
  }

  # Prepare harvest prior parameters -------------------------------------------

  if (temporal_name == "season") {
    # TODO: Allow for more years--currently for exactly 40 years
    harvest_rates <- harvest_rates %>%
      dplyr::filter(.data$year %in% 1979:2018) %>%
      dplyr::arrange(.data$spatial,  .data$number, .data$year) %>%
      dplyr::group_by(.data$spatial, .data$number) %>%
      dplyr::mutate(index = rep(1:8, each = 5)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$spatial, .data$number, .data$index) %>%
      dplyr::mutate(harvest_mean = mean(.data$harvest_rate)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        .data$spatial,
        .data$name,
        .data$short,
        .data$number,
        .data$index,
        .data$harvest_mean
      ) %>%
      dplyr::slice(rep(1:dplyr::n(), each = 4)) %>%
      dplyr::group_by(.data$spatial, .data$number) %>%
      dplyr::mutate(index = 1:32) %>%
      dplyr::ungroup() %>%
      dplyr::rename(harvest_rate = .data$harvest_mean)
  } else {
    harvest_rates <- harvest_rates %>%
      dplyr::filter(.data$year %in% 1979:2018) %>%
      dplyr::arrange(.data$spatial,  .data$number, .data$year) %>%
      dplyr::group_by(.data$spatial, .data$number) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  h_dim <- c(harvest_group_max, harvest_time_max, length(area_list))
  h_prior_mean <- array(NA_real_, dim = h_dim)

  h_prior_mean_matrix <- harvest_rates %>%
    dplyr::filter(.data$spatial == spatial_name) %>%
    dplyr::select(.data$short, .data$index, .data$harvest_rate) %>%
    tidyr::pivot_wider(
      names_from = .data$short,
      values_from = .data$harvest_rate
    ) %>%
    dplyr::select(-.data$index) %>%
    as.matrix()

  for (i in seq_len(harvest_group_max)) {
    h_prior_mean[i, , ] <- h_prior_mean_matrix
  }

  # Prepare random walk standard deviation prior parameters --------------------

  if (random_walk > 0 & movement_time_max > 1) {
    # Sigma prior mean
    if (length(sigma_prior_mean)  == 1) {
      sigma_prior_mean <- rep(sigma_prior_mean, length(area_list))
    } else if (length(sigma_prior_mean) == length(area_list)) {
      sigma_prior_mean <- sigma_prior_mean
    } else {
      stop("length(sigma_prior_mean) must be 1 or length(area_list)")
    }
    # Sigma prior sd
    if (length(sigma_prior_sd)  == 1) {
      sigma_prior_sd <- rep(sigma_prior_sd, length(area_list))
    } else if (length(sigma_prior_sd) == length(area_list)) {
      sigma_prior_sd <- sigma_prior_sd
    } else {
      stop("length(sigma_prior_mean) must be 1 or length(area_list)")
    }
  } else {
    sigma_prior_mean <- numeric(0)
    sigma_prior_sd <- numeric(0)
  }

  # Prepare movement time index ------------------------------------------------

  if (temporal_name == "average") {
    movement_time_index <- rep(1, study_time_max)
  } else if (temporal_name == "season") {
    movement_time_index <- rep(
      x = seq_len(year_time_max),
      times = ceiling(study_time_max / year_time_max)
    )[seq_len(study_time_max)]
  } else if (temporal_name == "year") {
    movement_time_index <- rep(
      x = seq_len(ceiling(study_time_max / year_time_max)),
      each = year_time_max
    )[seq_len(study_time_max)]
  } else {
    stop("temporal_name must be one of 'average', 'season' or 'year'")
  }

  # Prepare harvest time index -------------------------------------------------

  if (temporal_name == "average") {
    harvest_time_index <- rep(
      x = seq_len(ceiling(study_time_max / year_time_max)),
      each = year_time_max
    )[seq_len(study_time_max)]
  } else if (temporal_name == "season") {
    harvest_time_index <- (seq_len(harvest_time_max) %>%
      rep(each = 5) %>%
      array(c(5, year_time_max, harvest_time_max / year_time_max)) %>%
      aperm(c(2, 1, 3)) %>%
      as.vector())[seq_len(study_time_max)]
  } else if (temporal_name == "year") {
    harvest_time_index <- rep(
      x = seq_len(ceiling(study_time_max / year_time_max)),
      each = year_time_max
    )[seq_len(study_time_max)]
  } else {
    stop("temporal_name must be one of 'average', 'season' or 'year'")
  }

  # Prepare reporting time index -----------------------------------------------

  reporting_time_index <- rep(1, study_time_max)

  # Prepare harvest group index ------------------------------------------------

  harvest_group_index <- seq_len(length(group_list))

  # Prepare movement index matrix ----------------------------------------------

  if (length(area_list) == 3) {
    # Movement between sequential areas at each time step
    z <- mmmstan::movement_index(n = 3, pattern = 0)
  } else if (length(area_list) == 6) {
    # Movement between sequential areas at each time step
    z <- mmmstan::movement_index(n = 6, pattern = 0)
  } else if (length(area_list) == 8) {
    # Movement between sequential areas plus EG-BC at each time step
    z <- mmmstan::movement_index(
      n = 8,
      pattern = 0,
      allow = matrix(c(5, 7, 7, 5), nrow = 2, byrow = TRUE)
    )
  } else if (length(area_list) == 10) {
    # Movement between sequential areas plus EG-NB at each time step
    z <- mmmstan::movement_index(
      n = 10,
      pattern = 0,
      allow = matrix(c(5, 7, 7, 5), nrow = 2, byrow = TRUE)
    )
  } else {
    stop("length(area_list) must be 3, 6, 8, or 10")
  }

  # Assemble data list ---------------------------------------------------------

  data_list <- list(
    A = length(area_list),
    G_released = length(group_list),
    G_harvest = harvest_group_max,
    T_released = released_time_max,
    T_liberty = liberty_time_max,
    T_study = study_time_max,
    T_movement = movement_time_max,
    T_harvest = harvest_time_max,
    T_reporting = reporting_time_max,
    T_year = year_time_max,
    m = natural_mortality_rate,
    u = (1 - tag_loss_rate_initial),
    v = tag_loss_rate_ongoing,
    w = reporting_rates,
    x = tags$x,
    y = tags$y,
    z = z,
    p_time_index = array(movement_time_index),
    h_time_index = array(harvest_time_index),
    w_time_index = array(reporting_time_index),
    h_group_index = array(harvest_group_index),
    random_walk = random_walk,
    h_prior_mean = h_prior_mean,
    h_prior_sd = array(h_prior_sd, dim = h_dim),
    phi_prior_mean = phi_prior_mean,
    phi_prior_sd = phi_prior_sd,
    sigma_prior_mean = sigma_prior_mean,
    sigma_prior_sd = sigma_prior_sd,
    p_fudge = movement_rate_fudge,
    y_fudge = predicted_tags_fudge
  )

  # Fit ------------------------------------------------------------------------

  cat("\n- fitting movement model\n\n")
  mmmstan::mmmfit(
    data = data_list,
    chains = chains,
    step_size = nuts_step_size,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    use_reduce_sum = use_reduce_sum,
    threads_per_chain = threads_per_chain,
    ...
  )
}
