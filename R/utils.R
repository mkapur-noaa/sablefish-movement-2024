create_abundance_exchange <- function (abundance,
                                       movement_rate,
                                       index = NULL,
                                       n_draws = 1000) {

  # Check arguments ------------------------------------------------------------

  # Create abundance draws array of matrices -----------------------------------

  # Create movement rate draws array of matrices -------------------------------

  # Compute abundance exchange draws array of matrices -------------------------

  # Compute abundance exchange means tibble ------------------------------------

  # Compute abundance exchange sds tibble --------------------------------------

  # Compute values tibble ------------------------------------------------------

  # Return values --------------------------------------------------------------

}

create_percent_attributable <- function (abundance,
                                         movement_rate,
                                         index = NULL,
                                         n_draws = 1000) {

  # Check arguments ------------------------------------------------------------

  # Create abundance draws array of matrices -----------------------------------

  # Create movement rate draws array of matrices -------------------------------

  # Compute percent attributable draws array of matrices -----------------------

  # Compute percent attributable means tibble ----------------------------------

  # Compute percent attributable sds tibble ------------------------------------

  # Compute values tibble ------------------------------------------------------

  # Return values --------------------------------------------------------------

}


# Outdated template for new versions
create_abundance_exchange <- function (abundance, movement_mean) {

  # Check arguments ------------------------------------------------------------

  # Assemble movement mean matrix ----------------------------------------------

  # Declare matrices
  mmean <- matrix(0.0, nrow = 3, ncol = 3)
  m5 <- matrix(0.0, nrow = 3, ncol = 3)
  m95 <- matrix(0.0, nrow = 3, ncol = 3)
  # Populate matrices
  for (i in seq_len(nrow(movement_mean))) {
    mmean[movement_mean$x[i], movement_mean$y[i]] <- movement_mean$mean[i]
    m5[movement_mean$x[i], movement_mean$y[i]] <- movement_mean$q5[i]
    m95[movement_mean$x[i], movement_mean$y[i]] <- movement_mean$q95[i]
  }

  # Assemble abundance array ---------------------------------------------------

  # Augment abundance
  abundance <- abundance %>%
    dplyr::group_by(.data$region_name) %>%
    dplyr::mutate(t = dplyr::row_number()) %>%
    dplyr::ungroup()
  # Declare arrays
  abundance_array <- array(0.0, dim = c(max(abundance$t), 3, 3))
  # Populate array
  for (i in seq_len(nrow(abundance))) {
    abundance_array[
      abundance$t[i],
      abundance$region_number[i],
      abundance$region_number[i]
    ] <- abundance$total[i]
  }

  # Assemble abundance exchange array ------------------------------------------

  # Declare array
  exchange_mean <- array(0.0, dim = c(max(abundance$t), 3, 3))
  exchange_q5 <- array(0.0, dim = c(max(abundance$t), 3, 3))
  exchange_q95 <- array(0.0, dim = c(max(abundance$t), 3, 3))
  # Populate array
  for (i in seq_len(dim(abundance_array)[1])) {
    exchange_mean[i,,] <- as.matrix(abundance_array[i,,]) %*% mmean
    exchange_q5[i,,] <- as.matrix(abundance_array[i,,]) %*% m5
    exchange_q95[i,,] <- as.matrix(abundance_array[i,,]) %*% m95
  }

  # Assemble exchange sum ------------------------------------------------------

  # Declare
  exchange_sum <- array(0.0, dim = c(max(abundance$t), 4))
  exchange_s5 <- array(0.0, dim = c(max(abundance$t), 4))
  exchange_s95 <- array(0.0, dim = c(max(abundance$t), 4))
  # Populate
  for (i in seq_len(max(abundance$t))) {
    # Mean
    exchange_sum[i, 1] <- exchange_mean[i, 1, 2] + exchange_mean[i, 1, 3] # AK S
    exchange_sum[i, 2] <- exchange_mean[i, 2, 1] + exchange_mean[i, 3, 1] # BC N
    exchange_sum[i, 3] <- exchange_mean[i, 2, 3] + exchange_mean[i, 1, 3] # BC S
    exchange_sum[i, 4] <- exchange_mean[i, 3, 1] + exchange_mean[i, 3, 2] # CC N
    # Q5
    exchange_s5[i, 1] <- exchange_q5[i, 1, 2] + exchange_q5[i, 1, 3] # AK S
    exchange_s5[i, 2] <- exchange_q5[i, 2, 1] + exchange_q5[i, 3, 1] # BC N
    exchange_s5[i, 3] <- exchange_q5[i, 2, 3] + exchange_q5[i, 1, 3] # BC S
    exchange_s5[i, 4] <- exchange_q5[i, 3, 1] + exchange_q5[i, 3, 2] # CC N
    # Q95
    exchange_s95[i, 1] <- exchange_q95[i, 1, 2] + exchange_q95[i, 1, 3] # AK S
    exchange_s95[i, 2] <- exchange_q95[i, 2, 1] + exchange_q95[i, 3, 1] # BC N
    exchange_s95[i, 3] <- exchange_q95[i, 2, 3] + exchange_q95[i, 1, 3] # BC S
    exchange_s95[i, 4] <- exchange_q95[i, 3, 1] + exchange_q95[i, 3, 2] # CC N
  }

  # To tibble
  exchange_sum_tibble <- exchange_sum %>%
    magrittr::set_colnames(c("ak s", "bc n", "bc s", "cc n")) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "direction",
      values_to = "mean"
    ) %>%
    dplyr::arrange(.data$direction) %>%
    dplyr::mutate(year = rep(1979:2018, 4)) %>%
    dplyr::relocate(.data$year, .before = 1)
  # Q5
  exchange_s5_tibble <- exchange_s5 %>%
    magrittr::set_colnames(c("ak s", "bc n", "bc s", "cc n")) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "direction",
      values_to = "q5"
    ) %>%
    dplyr::arrange(.data$direction) %>%
    dplyr::mutate(year = rep(1979:2018, 4)) %>%
    dplyr::relocate(.data$year, .before = 1)
  # Q95
  exchange_s95_tibble <- exchange_s95 %>%
    magrittr::set_colnames(c("ak s", "bc n", "bc s", "cc n")) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "direction",
      values_to = "q95"
    ) %>%
    dplyr::arrange(.data$direction) %>%
    dplyr::mutate(year = rep(1979:2018, 4)) %>%
    dplyr::relocate(.data$year, .before = 1)

  # Assemble abundance exchange ------------------------------------------------

  # Declare
  abundance_exchange <- exchange_sum_tibble %>%
    dplyr::left_join(exchange_s5_tibble, by = c("year", "direction")) %>%
    dplyr::left_join(exchange_s95_tibble, by = c("year", "direction"))


  # Return value ---------------------------------------------------------------

  return(abundance_exchange)
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


