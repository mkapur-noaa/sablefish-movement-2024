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
