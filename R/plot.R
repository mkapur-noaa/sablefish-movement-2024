

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
                      xmin = 169,
                      ymin = 31,
                      xmax = 241,
                      ymax = 65.5,
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

  # Define outline -------------------------------------------------------------

  outline <- tibble::tibble(
    x = c(xmin, xmin, xmax, xmax, xmin),
    y = c(ymin, ymax, ymax, ymin, ymin)
  )

  # Define inset ---------------------------------------------------------------

  inset <- ggplot2::ggplot() +
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
    ggplot2::coord_sf(
      xlim = c(130, 300),
      ylim = c(8, 80)
    ) +
    ggplot2::geom_polygon(
      data = outline,
      mapping = ggplot2::aes(x = x, y = y),
      fill = NA,
      color = "grey30",
      size = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

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
      mapping = ggplot2::aes(X, Y, label = region_short),
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
      height = grid::unit(0.25, "npc"),
      width = grid::unit(0.2, "npc"),
      pad_x = grid::unit(0.8, "npc"),
      pad_y = grid::unit(0.72, "npc"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )  +
    patchwork::inset_element(
      inset,
      left = grid::unit(0, "npc"),
      bottom = grid::unit(0, "npc"),
      right = grid::unit(0.4, "npc"),
      top = grid::unit(0.4, "npc")
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
