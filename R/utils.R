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


