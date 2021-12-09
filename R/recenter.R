#' Recenter A Simple Features Object
#'
#' @description https://github.com/AustralianAntarcticDivision/SOmap/issues/34#issue-429072110
#'
#' @param x Argument
#' @param clon Argument
#' @param ... Additional arguments
#' @param tryfix Argument
#'
#' @return A recentered simple features object
#' @export
#'
st_recenter <- function(x, clon = NULL, ..., tryfix = TRUE) {
  if (is.null(clon)) return(x)
  if (!sf::st_is_longlat(x)) {
    stop("recentering not appropriate for non longlat data")
  }
  # Save the crs while we do our munging
  crs <- sf::st_crs(x)
  x <- sf::st_set_crs(x, NA)
  # Try to fix problematic geometry
  if (tryfix) {
    if (all(grepl("POLYGON", sf::st_geometry_type(x)))) {
      x <- suppressWarnings(sf::st_buffer(sf::st_as_sf(x), 0))
      x <- sf::st_wrap_dateline(
        x,
        options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
      # My x was no longer valid: LR added 2019-10-17
      x <- sf::st_make_valid(x)
    }
  }
  wbox <- sf::st_bbox(c(
    xmin = -180,
    ymin = -90,
    xmax = (clon)%%360 - 180,
    ymax = 90))
  west <- suppressWarnings(sf::st_crop(x, wbox))
  west <- sf::st_set_geometry(west, sf::st_geometry(west) + c(360, 0))
  east <- suppressWarnings(sf::st_crop(
    x,
    sf::st_bbox(c(
      xmin = (clon)%%360 - 180,
      ymin = -90,
      xmax = 180,
      ymax = 90))))
  xx <- rbind(
    west,
    east
  )
  # Ensure geometries are of consistent type
  xx <- sf::st_cast(xx)
  bb <- sf::st_bbox(xx)
  # Set crs
  sf::st_set_crs(xx, crs)
}
