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

#' Write An Object To Data And Return The Path
#'
#' @param ... Unquoted name of an existing object to write to \code{data/}.
#'   See \code{?usethis::use_data()}.
#'
#' @return [character()] file path
#' @export
#'
write_data <- function (...) {
  if (...length() != 1) stop("write_data() takes exactly 1 argument")
  args <- list(...)
  if (is.null(names(args))) {
    name <- as.character(substitute(...))
  } else {
    name <- names(args)[1]
  }
  assign(x = name, value = ..1)
  usethis::use_data(..., overwrite = TRUE)
  file.path("data", fs::path_ext_set(name, ".rda"))
}
