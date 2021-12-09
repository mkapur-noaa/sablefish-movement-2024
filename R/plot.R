

plot_map <- function (regions,
                      plot_name,
                      size_short = 2,
                      size_line = 0.25,
                      color_land = "white",
                      color_ocean = "grey98",
                      color_region = "grey30",
                      fill_land = "white",
                      fill_ocean = "grey95",
                      fill_region = "grey85",
                      xmin = 170,
                      ymin = 30,
                      xmax = 240,
                      ymax = 66,
                      width = 6,
                      height = 4) {

  # Define centroid ------------------------------------------------------------

  sf::sf_use_s2(FALSE)
  regions <- regions %>%
    cbind(sf::st_coordinates(sf::st_centroid(.$geometry)))

  # Define coastline -----------------------------------------------------------

  coastline <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define land ----------------------------------------------------------------

  land <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define plot ----------------------------------------------------------------

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = land,
      color = color_land,
      fill = fill_land,
      lwd = 0.1
    ) +
    ggplot2::geom_sf(
      data = coastline,
      color = color_region,
      fill = NA,
      lwd = 0.25
    ) +
    ggplot2::geom_sf(
      data = regions,
      col = color_region,
      fill = fill_region,
      size = size_line
    ) +
    ggplot2::geom_label(
      data = regions,
      mapping = aes(X, Y, label = region_short),
      size = size_short,
      label.r = grid::unit(0.05, "lines"),
      label.size = 0.125,
      label.padding = grid::unit(0.15, "lines"),
      nudge_x = c(0, 0, 0, 0, 0, 0, 0, -1),
      nudge_y = c(0, 0, 0, -1, 0.5, 0.25, 0, 0.25)
    ) +
    ggplot2::coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    ) +
    ggspatial::annotation_north_arrow(
      pad_x = grid::unit(0.5, "in"),
      pad_y = grid::unit(0.5, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_line(color = color_ocean),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(plot_name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", plot_name, ".png"))
}
