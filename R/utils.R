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
  # Instantiate abundance exchange matrix
  m_exchange <- matrix(0, nrow = 9 * n_years, ncol = 6)
  colnames(m_exchange) <- c("index", "year", "regions", "mean", "q5", "q95")
  # Populate abundance exchange matrix
  row_count <- 0
  for (n in seq_len(n_years)) {
    for (y in 1:3) {
      for (x in 1:3) {
        # Increment row count
        row_count <- row_count + 1
        # Assign values
        m_exchange[row_count, 1] <- n
        m_exchange[row_count, 2] <- n + min(years) - 1
        m_exchange[row_count, 3] <- m_regions[x, y]
        m_exchange[row_count, 4] <- mean(exchange_array[x, y, , n])
        m_exchange[row_count, 5] <- quantile(exchange_array[x, y, , n], 0.05)
        m_exchange[row_count, 6] <- quantile(exchange_array[x, y, , n], 0.95)
      }
    }
  }
  # As tibble: | year | regions | mean | q5 | q95 |
  abundance_exchange <- m_exchange %>%
    tibble::as_tibble() %>%
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
  # Instantiate abundance exchange matrix
  m_percent <- matrix(0, nrow = 9 * n_years, ncol = 6)
  colnames(m_percent) <- c("index", "year", "regions", "mean", "q5", "q95")
  # Populate percent attributable matrix
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
        m_percent[row_count, 1] <- n
        m_percent[row_count, 2] <- n + min(years) - 1
        m_percent[row_count, 3] <- m_regions[x, y]
        m_percent[row_count, 4] <- mean(pct_draws)
        m_percent[row_count, 5] <- quantile(pct_draws, 0.05)
        m_percent[row_count, 6] <- quantile(pct_draws, 0.95)
      }
    }
  }
  # As tibble: | year | regions | mean | q5 | q95 |
  percent_attributable <- m_percent %>%
    tibble::as_tibble() %>%
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
#'
#' @return [character()]
#'
#' @examples
#' x <-seq(0, 0.1, 0.01)
#' round_to_character(x, 2)
#'
round_to_character <- function (x, digits = 2) {
  y <- character(length = length(x))
  ind_low <- which(x < 10^(-digits))
  ind_high <- which(x >= 10^(-digits))
  # Assign
  y[ind_low] <- paste0("<", sub(".", "", 10^(-digits)))
  # y[ind_high] <- as.character(round(x[ind_high], digits))
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

# Current above here -----------------------------------------------------------



#' Convert Region Number Vector to Short Name Factor
#'
#' @param n [numeric()]
#'
#' @return [factor()]
#' @export
#'
number_to_short <- function (n) {
  if (max(n) == 3) {
    short <- factor(
      c("AK", "BC", "CC")[n],
      levels = c("AK", "BC", "CC")
    )
  } else if (max(n) == 6) {
    short <- factor(
      c("WAK", "EAK", "NBC", "SBC", "NCC", "SCC")[n],
      levels = c("WAK", "EAK", "NBC", "SBC", "NCC", "SCC")
    )
  } else if (max(n) == 8) {
    short <- factor(
      c("BS", "AI", "WG", "CG", "EG", "SE", "BC", "CC")[n],
      levels = c("BS", "AI", "WG", "CG", "EG", "SE", "BC", "CC")
    )
  } else if (max(n) == 10) {
    short <- factor(
      c("BS", "AI", "WG", "CG", "EG", "SE", "NB", "SB", "NC", "SC")[n],
      levels = c("BS", "AI", "WG", "CG", "EG", "SE", "NB", "SB", "NC", "SC")
    )
  } else {
    stop("max(n) must be 3, 6, 8, or 10")
  }
  return(short)
}

#' Modulo Function
#'
#' @param a [numeric()] [vector()]
#' @param b [numeric()] [vector()]
#' @param zeros [logical()] replace zeros by \code{b}?
#'
#' @return [numeric()]
#' @export
#'
#' @examples
#' # Standard modulo operation
#' mod(1:10, 8)
#'
#' # Replace zeros by \code{b}
#' mod(1:10, 8, zeros = FALSE)
#'
mod <- function (a, b, zeros = TRUE) {
  ans <- a - (b * floor(a / b))
  if (!zeros) {
    ans[which(ans == 0)] <- b
  }
  return(ans)
}

#' Round to Character
#'
#' @param x [numeric()] Value to round
#' @param digits [numeric] Number of digits
#'
#' @return [character()]
#'
#' @examples
#' x <-seq(0, 0.1, 0.01)
#' round_to_character(x, 2)
#'
round_to_character <- function (x, digits = 2) {
  y <- character(length = length(x))
  ind_low <- which(x < 10^(-digits))
  ind_high <- which(x >= 10^(-digits))
  # Assign
  y[ind_low] <- paste0("<", sub(".", "", 10^(-digits)))
  # y[ind_high] <- as.character(round(x[ind_high], digits))
  # Keep trailing zeros
  y[ind_high] <- sprintf(
    paste0("%.", digits, "f"),
    round(x[ind_high], digits)
  )
  # Return
  y
}

#' Time To Label
#'
#' @param n [numeric()]
#'
#' @return [vector()]
#' @export
#'
time_to_label <- function (n) {
  if (max(n) == 4) {
    label <- factor(
      c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[n],
      levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
  } else {
    label <- c(1979:2018)[n]
  }
  return(label)
}

#' Convert Released Time Unit To Time Steps Per Year
#'
#' @param unit [character()] one of \code{'year'}, \code{'quarter'}, or
#'   \code{'month'}.
#'
#' @return [numeric()] number of time steps per year
#' @export
#'
#' @examples
#' unit_to_time("year")
#' unit_to_time("quarter")
#' unit_to_time("month")
#'
unit_to_time <- function (unit) {
  c(1L, 4L, 12L)[which(c("year", "quarter", "month") == unit)]
}


