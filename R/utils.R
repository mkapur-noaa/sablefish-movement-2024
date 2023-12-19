create_abundance_exchange <- function (abundance,
                                       movement_list,
                                       index_list = NULL,
                                       years = 1979:2017,
                                       n_draws = 1000) {

  # Check arguments ------------------------------------------------------------

  # Augment abundance ----------------------------------------------------------

  abundance <- abundance %>%
    dplyr::mutate(ind_year = year - min(years) + 1) %>%
    dplyr::filter(year %in% years)

  # Create values --------------------------------------------------------------

  n_rows <- nrow(abundance)
  n_years <- length(years)
  n_blocks <- length(movement_list)

  # Create abundance draws array of matrices -----------------------------------

  # Instantiate abundance array
  abundance_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate abundance array
  for (i in seq_len(n_rows)) {
    # For clarity
    x <- abundance$region_number[i]
    n <- abundance$ind_year[i]
    a_mean <- abundance$abundance[i]
    a_sd <- abundance$sd[i]
    # Assign value
    abundance_array[x, x, , n] <- stats::rnorm(n_draws, a_mean, a_sd)
  }

  # Create movement rate draws array of matrices -------------------------------

  # Instantiate movement array
  movement_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate movement array
  for (i in seq_len(n_blocks)) {
    for (j in seq_along(index_list[[i]])) {
      for (k in 1:9) { # 9 rows each block
        # For clarity
        x <- movement_list[[i]]$x[k]
        y <- movement_list[[i]]$y[k]
        n <- index_list[[i]][j]
        m_mean <- movement_list[[i]]$mean[k]
        m_sd <- movement_list[[i]]$sd[k]
        # Assign value
        movement_array[x, y, , n] <- stats::rnorm(n_draws, m_mean, m_sd)
      }
    }
  }

  # Compute abundance exchange draws array of matrices -------------------------

  # Instantiate abundance exchange array
  exchange_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate abundance exchange array
  for (n in seq_len(n_years)) {
    for (d in seq_len(n_draws)) {
      exchange_array[ , , d, n] = abundance_array[ , , d, n] %*%
        movement_array[ , , d, n]
    }
  }

  # Create tibble --------------------------------------------------------------

  # Create regions matrix
  v_regions <- c("akak","akbc","akcc","bcak","bcbc","bccc","ccak","ccbc","cccc")
  m_regions <- matrix(v_regions, nrow = 3, ncol = 3, byrow = TRUE)
  # Instantiate tibble
  t_exchange <- tibble::tibble(
    index = NA_integer_,
    year = NA_integer_,
    regions = NA_character_,
    mean = NA_real_,
    q5 = NA_real_,
    q95 = NA_real_,
    .rows = 9 * n_years
  )
  # Populate tibble
  row_count <- 0
  for (n in seq_len(n_years)) {
    for (y in 1:3) {
      for (x in 1:3) {
        # Increment row count
        row_count <- row_count + 1
        # Assign values
        t_exchange$index[row_count] <- n
        t_exchange$year[row_count] <- n + min(years) - 1
        t_exchange$regions[row_count] <- m_regions[x, y]
        t_exchange$mean[row_count] <- mean(exchange_array[x, y, , n])
        t_exchange$q5[row_count] <- quantile(exchange_array[x, y, , n], 0.05)
        t_exchange$q95[row_count] <- quantile(exchange_array[x, y, , n], 0.95)
      }
    }
  }
  # As tibble: | year | regions | mean | q5 | q95 |
  abundance_exchange <- t_exchange %>%
    dplyr::arrange(regions, year) %>%
    dplyr::ungroup() %>%
    dplyr::select(-1)

  # Return tibble --------------------------------------------------------------

  return(abundance_exchange)

}

create_percent_attributable <- function (abundance,
                                         movement_list,
                                         index_list = NULL,
                                         years = 1979:2017,
                                         n_draws = 1000) {

  # Check arguments ------------------------------------------------------------

  # Augment abundance ----------------------------------------------------------

  abundance <- abundance %>%
    dplyr::mutate(ind_year = year - min(years) + 1) %>%
    dplyr::filter(year %in% years)

  # Create values --------------------------------------------------------------

  n_rows <- nrow(abundance)
  n_years <- length(years)
  n_blocks <- length(movement_list)

  # Create abundance draws array of matrices -----------------------------------

  # Instantiate abundance array
  abundance_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate abundance array
  for (i in seq_len(n_rows)) {
    # For clarity
    x <- abundance$region_number[i]
    n <- abundance$ind_year[i]
    a_mean <- abundance$abundance[i]
    a_sd <- abundance$sd[i]
    # Assign value
    abundance_array[x, x, , n] <- stats::rnorm(n_draws, a_mean, a_sd)
  }

  # Create movement rate draws array of matrices -------------------------------

  # Instantiate movement array
  movement_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate movement array
  for (i in seq_len(n_blocks)) {
    for (j in seq_along(index_list[[i]])) {
      for (k in 1:9) { # 9 rows each block
        # For clarity
        x <- movement_list[[i]]$x[k]
        y <- movement_list[[i]]$y[k]
        n <- index_list[[i]][j]
        m_mean <- movement_list[[i]]$mean[k]
        m_sd <- movement_list[[i]]$sd[k]
        # Assign value
        movement_array[x, y, , n] <- stats::rnorm(n_draws, m_mean, m_sd)
      }
    }
  }

  # Compute abundance exchange draws array of matrices -------------------------

  # Instantiate abundance exchange array
  exchange_array <- array(0, dim = c(3, 3, n_draws, n_years))
  # Populate abundance exchange array
  for (n in seq_len(n_years)) {
    for (d in seq_len(n_draws)) {
      exchange_array[ , , d, n] = abundance_array[ , , d, n] %*%
        movement_array[ , , d, n]
    }
  }

  # Create tibble --------------------------------------------------------------

  # Create regions matrix
  v_regions <- c("akak","akbc","akcc","bcak","bcbc","bccc","ccak","ccbc","cccc")
  m_regions <- matrix(v_regions, nrow = 3, ncol = 3, byrow = TRUE)
  # Instantiate tibble
  t_percent <- tibble::tibble(
    index = NA_integer_,
    year = NA_integer_,
    regions = NA_character_,
    mean = NA_real_,
    q5 = NA_real_,
    q95 = NA_real_,
    .rows = 9 * n_years
  )
  # Populate tibble
  row_count <- 0
  for (n in seq_len(n_years)) {
    for (y in 1:3) {
      # Compute denominator: abundance draws in region y (col) after movement
      v_denom <- exchange_array[1, y, , n] +
        exchange_array[2, y, , n] +
        exchange_array[3, y, , n]
      # Compute values: percent abundance draws in y attributable to x
      for (x in 1:3) {
        # Increment row count
        row_count <- row_count + 1
        # Compute values
        pct_draws <- exchange_array[x, y, , n] / v_denom
        # Assign values
        t_percent$index[row_count] <- n
        t_percent$year[row_count] <- n + min(years) - 1
        t_percent$regions[row_count] <- m_regions[x, y]
        t_percent$mean[row_count] <- mean(pct_draws)
        t_percent$q5[row_count] <- quantile(pct_draws, 0.05)
        t_percent$q95[row_count] <- quantile(pct_draws, 0.95)
      }
    }
  }
  # As tibble: | year | regions | mean | q5 | q95 |
  percent_attributable <- t_percent %>%
    dplyr::arrange(regions, year) %>%
    dplyr::ungroup() %>%
    dplyr::select(-1)

  # Return tibble --------------------------------------------------------------

  return(percent_attributable)
}


#' Number To Region
#'
#' @param x [numeric()]
#' @param regions [character()]
#'
#' @return [character()]
#' @export
#'
#' @examples
#'
#' number_to_region(c(1,1,2,3), LETTERS)
#'
number_to_region <- function (x, regions) {
  regions[x]
}

#' Regions 3 to regions 8
#'
#' @param x [numeric()]
#'
#' @return [numeric()]
#' @export
#'
#' @examples
#'
#' regions_8_to_3(c(1:8))
#'
regions_8_to_3 <- function(x) {
  c(rep(1, 6), 2, 3)[x]
}

#' Round to Character
#'
#' @param x [numeric()] Value to round
#' @param digits [numeric] Number of digits
#' @param tex [logical] avoid "<" character?
#'
#' @return [character()]
#'
#' @examples
#' x <-seq(0, 0.1, 0.01)
#' round_to_character(x, 2)
#' round_to_character(x, 2, tex = TRUE)
#'
round_to_character <- function (x, digits = 2, tex = FALSE) {
  y <- character(length = length(x))
  ind_low <- which(x < 10^(-digits))
  ind_high <- which(x >= 10^(-digits))
  # Assign
  if(!tex) {
    y[ind_low] <- paste0("<", sub(".", "", 10^(-digits)))
  } else {
    y[ind_low] <- sprintf(
      paste0("%.", digits, "f"),
      round(x[ind_low], digits)
    )
  }
  # Keep trailing zeros
  y[ind_high] <- sprintf(
    paste0("%.", digits, "f"),
    round(x[ind_high], digits)
  )
  # Return
  y
}

#' Read File From Path
#'
#' @param path [character()] file path
#'
#' @return the object at the file path
#' @export
#'
read_from_path <- function (path) {
  envir <- environment()
  data_name <- load(path, envir = envir)
  get(data_name)
}

#' Write An Object And Return The Path
#'
#' @param path [character()] folder path
#' @param ... Unquoted name of an existing object to write to \code{data/}.
#'
#' @return [character()] file path
#' @export
#'
write_data <- function (..., path = "data") {
  if (...length() != 1) stop("write_data() takes exactly 1 argument")
  args <- list(...)
  if (is.null(names(args))) {
    name <- as.character(substitute(...))
  } else {
    name <- names(args)[1]
  }
  assign(x = name, value = ..1)
  save(..., file = paste0(path, "/", name, ".rda"))
  file.path(path, fs::path_ext_set(name, ".rda"))
}

write_movement_rate_csv <- function (m, name, path = "ms/tabs") {

  # Check arguments ------------------------------------------------------------

  # Define regions -------------------------------------------------------------

  if (max(m$x) == 3) {
    regions <- c("AK", "BC", "CC")
  } else if (max(m$x) == 6) {
    regions <- c("WAK", "EAK", "NBC", "SBC", "NCC", "SCC")
  } else if (max(m$x) == 8) {
    regions <- c("BS", "AI", "WG", "CG", "EG", "SE", "BC", "CC")
  } else {
    stop("move must have 3, 6, or 8 regions")
  }

  # Create tibble --------------------------------------------------------------

  rates <- m %>%
    dplyr::mutate(
      value = paste0(
        round_to_character(mean, 3, tex = TRUE),
        " (",
        round_to_character(q5, 3, tex = TRUE),
        "-",
        round_to_character(q95, 3, tex = TRUE),
        ")"
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = x,
      names_from = y,
      values_from = value
    ) %>%
    dplyr::mutate(x = number_to_region(x, regions)) %>%
    magrittr::set_colnames(c(" ", regions))

  # Write csv ------------------------------------------------------------------

  readr::write_csv(x = rates, file = paste0(path, "/", name, ".csv"))

  # Return extension -----------------------------------------------------------

  file.path(path, fs::path_ext_set(name, ".csv"))
}

#' Write Value to Latex Macro
#'
#' @param name [character()] Macro name
#' @param value [numeric()] Macro value
#' @param path [character()] File path
#' @param filename [character()] File name including extension
#'
#' @return NULL
#' @importFrom magrittr "%>%"
#' @export
#'
write_tex <- function (name,
                       value,
                       path = file.path("ms","vals"),
                       filename = "values.tex") {

  # Write latex macro ----------------------------------------------------------

  paste0("\\newcommand{\\", name, "}{", value, "}") %>%
    readr::write_lines(here::here(path, filename), append = TRUE)

  # Return NULL ----------------------------------------------------------------

  invisible()
}

#' Clear Latex Macros
#'
#' @return NULL
#' @export
#'
clear_tex <- function(path = file.path("ms", "vals"), filename = "values.tex") {
  # Clear tex
  readr::write_lines(NULL, here::here(path, filename), append = FALSE)
  # Return NULL
  invisible()
}

write_values_tex <- function (name_value_list = list(zero = 0),
                              path = file.path("ms", "vals"),
                              filename = "values.tex",
                              clear_first = FALSE) {

  # Check arguments ------------------------------------------------------------

  # TODO: Ensure names have no underscores

  # Clear first ----------------------------------------------------------------

  if (clear_first) clear_tex(path = path, filename = filename)

  # Write values ---------------------------------------------------------------

  purrr::map2(
    .x = names(name_value_list),
    .y = name_value_list,
    .f = write_tex,
    path = path,
    filename = filename
  )

  # Return path ----------------------------------------------------------------

  file.path(path, filename)
}
