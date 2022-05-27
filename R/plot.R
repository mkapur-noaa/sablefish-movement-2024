plot_map <- function (regions,
                      plot_name,
                      size_short = 2,
                      size_line = 0.25,
                      size_text = 2,
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
                      width = 90,
                      height = 60,
                      file_type = ".png") {

  # Define centroid ------------------------------------------------------------

  sf::sf_use_s2(FALSE)
  regions <- regions %>%
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(.$geometry))))

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

  xbuf <- 3
  ybuf <- 2.5

  outline <- tibble::tibble(
    x = c(xmin - xbuf, xmin - xbuf, xmax + xbuf, xmax + xbuf, xmin - xbuf),
    y = c(ymin - ybuf, ymax + ybuf, ymax + ybuf, ymin - ybuf, ymin - ybuf)
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
      color = "grey60",
      size = 0.5
    ) +
    ggsidekick::theme_sleek() +
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

  p1 <- ggplot2::ggplot() +
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
      style = ggspatial::north_arrow_fancy_orienteering(
        text_col = "grey60",
        line_col = "grey60",
        fill = c("white", "grey60")
      )
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = size_text),
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_line(color = color_ocean),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
    ) +
    ggplot2::annotation_custom(
      ggplot2::ggplotGrob(inset),
      xmin = xmin - 5.6,
      ymin = ymin - 4.5,
      xmax = 200,
      ymax = 45
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_heat <- function (data,
                       plot_name,
                       regions,
                       size_text = 5,
                       size_mean = 3,
                       nudge_mean = 0.15,
                       size_sd = 2,
                       nudge_sd = 0.15,
                       legend_name = "Movement rate",
                       xlab = NULL,
                       ylab = NULL,
                       xtext = TRUE,
                       ytext = TRUE,
                       margin_x = -15,
                       margin_y = -15,
                       width = 90,
                       height = 110,
                       file_type = ".png") {

  # Augment data ---------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(
      region_previous = factor(
        number_to_region(.data$x, regions),
        rev(regions)
      ),
      region_current = factor(
        number_to_region(.data$y, regions),
        regions
      )
    )

  # Construct geom object ------------------------------------------------------

  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$region_current,
      y = .data$region_previous,
      fill = .data$mean
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      width = 0.975,
      height = 0.975
    ) +
    # Use viridis
    ggplot2::scale_fill_viridis_c(
      begin = 0,
      end = 1,
      direction = 1,
      option = "plasma",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25)
    ) +
    # Add mean
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = round_to_character(.data$mean, 2),
        col = as.factor(ifelse(.data$mean >= 0.55, 0, 1))
      ),
      fontface = "plain",
      nudge_y = nudge_mean,
      size = size_mean
    ) +
    # Add ci
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        # label = paste0("(", round_to_character(.data$sd, 3), ")"),
        label = paste0(
          "(",
          round_to_character(.data$q5, 3),
          "-",
          round_to_character(.data$q95, 3),
          ")"
        ),
        col = as.factor(ifelse(.data$mean >= 0.55, 0, 1))
      ),
      fontface = "plain",
      nudge_y = -nudge_sd,
      size = size_sd
    ) +
    # Text to black and white
    ggplot2::scale_color_grey(start = 0, end = 1) +
    ggplot2::guides(
      col = "none",
      fill = if (is.null(legend_name)) {
        "none"
      } else {
        ggplot2::guide_colorbar(
          title.position = "top",
          title.hjust = 0.5
        )
      }
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_name) +
    # Theme
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = size_text),
      legend.position = ifelse(is.null(legend_name), "none", "bottom"),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      legend.key.width = grid::unit(0.1, "npc"),
      legend.key.height = grid::unit(0.03, "npc"),
      legend.spacing.y = grid::unit(0.01, "npc"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 2),
      axis.text.x = if (xtext) {
        ggplot2::element_text(margin = ggplot2::margin(t = margin_x))
      } else {
        NULL
      },
      axis.text.y = if (ytext) {
        ggplot2::element_text(margin = ggplot2::margin(r = margin_y))
      } else {
        NULL
      }
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_cols <- function (data,
                       plot_name,
                       regions,
                       xvar,
                       xlab,
                       ylab,
                       x_text,
                       x_breaks,
                       y_text,
                       y_breaks,
                       x_angle = 0,
                       hjust = 0.5,
                       vjust = 0.5,
                       size_title = 8,
                       size_strip = 8,
                       size_text = 8,
                       size_error = 0.2,
                       panel_spacing = 1,
                       xmin = 1,
                       xmax = 20,
                       ymin = 0.0,
                       ymax = 1.0,
                       width = 90,
                       height = 90,
                       dpi = 300,
                       file_type = ".png") {

  # Augment data ---------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(
      region_previous = factor(number_to_region(.data$x, regions), regions),
      region_current = factor(number_to_region(.data$y, regions), regions)
    )

  # Define plot ----------------------------------------------------------------

  p1 <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data[[xvar]],
        y = .data$mean
      )
    ) +
    ggplot2::geom_col(
      color = "white",
      size = 0.1
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$region_previous),
      cols = ggplot2::vars(.data$region_current),
      switch = "y"
    ) +
    ggplot2::scale_x_continuous(
      labels = x_text,
      breaks = x_breaks,
      limits = c(xmin, xmax)
    ) +
    ggplot2::scale_y_continuous(
      labels = y_text,
      breaks = y_breaks,
      position = "right"
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(panel_spacing, "mm"),
      strip.text.x = ggplot2::element_text(
        size = size_strip
      ),
      strip.text.y.left = ggplot2::element_text(
        size = size_strip,
        angle = 0
      ),
      axis.title = ggplot2::element_text(
        size = size_title
      ),
      axis.text.x = ggplot2::element_text(
        size = size_text,
        angle = x_angle,
        hjust = hjust,
        vjust = vjust
      ),
      axis.text.y = ggplot2::element_text(
        size = size_text
      )
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_exchange <- function (data,
                           plot_name,
                           size_hline = 0.5,
                           size_error = 0.5,
                           width = 90,
                           height = 100,
                           dpi = 300,
                           file_type = figure_type) {

  # Check arguments ------------------------------------------------------------

  # Annotations ----------------------------------------------------------------

  north_to_ak <- tibble::tibble(
    x = 2010,
    y = 45e5,
    label = "North to Alaska"
  )
  south_to_bc <- tibble::tibble(
    x = 2010,
    y = -21e6,
    label = "South to British Columbia"
  )
  north_to_bc <- tibble::tibble(
    x = 2010,
    y = 14e6,
    label = "North to British Columbia"
  )
  south_to_cc <- tibble::tibble(
    x = 2010,
    y = -3e6,
    label = "South to California Current"
  )

  # Split data -----------------------------------------------------------------

  ak_s <- dplyr::filter(data, .data$direction == "ak s", .data$year %in% 1979:2017)
  bc_n <- dplyr::filter(data, .data$direction == "bc n", .data$year %in% 1979:2017)
  bc_s <- dplyr::filter(data, .data$direction == "bc s", .data$year %in% 1979:2017)
  cc_n <- dplyr::filter(data, .data$direction == "cc n", .data$year %in% 1979:2017)
  # Line data
  line_1 <- tibble::tibble(
    year = ak_s$year,
    net_mean = bc_n$mean - ak_s$mean,
    net_q5 = bc_n$q5 - ak_s$q5,
    net_q95 = bc_n$q95 - ak_s$q95
  )
  line_2 <- tibble::tibble(
    year = bc_s$year,
    net_mean = cc_n$mean - bc_s$mean,
    net_q5 = cc_n$q5 - bc_s$q5,
    net_q95 = cc_n$q95 - bc_s$q95
  )

  # Define plots ---------------------------------------------------------------

  # Ak and BC
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bc_n,
      ggplot2::aes(x = .data$year, y = .data$mean)
    ) +
    ggplot2::geom_col(
      data = ak_s,
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      size = size_hline,
      col = "white"
    ) +
    ggplot2::geom_line(
      data = line_1,
      ggplot2::aes(x = .data$year, y = .data$net_mean)
    ) +
    ggplot2::geom_errorbar(
      data = bc_n,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = ak_s,
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = line_1,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$net_q5,
        ymax = .data$net_q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-22e6, 5e6),
      breaks = seq(-2e7, 5e6, 5e6),
      labels = c(20, 15, 10, 5, 0, 5)
    ) +
    ggplot2::geom_text(
      data = north_to_ak,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = south_to_bc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )
  # BC and CC
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = cc_n,
      ggplot2::aes(x = .data$year, y = .data$mean)
    ) +
    ggplot2::geom_col(
      data = bc_s,
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      size = size_hline,
      col = "white"
    ) +
    ggplot2::geom_line(
      data = line_2,
      ggplot2::aes(x = .data$year, y = .data$net_mean)
    ) +
    ggplot2::geom_errorbar(
      data = cc_n,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = bc_s,
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = line_2,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$net_q5,
        ymax = .data$net_q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-4e6, 15e6),
      breaks = seq(-5e6, 15e6, 5e6),
      labels = c(5, 0, 5, 10, 15)
    ) +
    ggplot2::geom_text(
      data = north_to_bc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = south_to_cc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )

  # Assemble panel figure ------------------------------------------------------

  p0 <- ggpubr::ggarrange(
    p1, p2,
    heights = c(27, 19),
    nrow = 2
  ) %>%
    ggpubr::annotate_figure(
      left = ggpubr::text_grob(
        "Abundance exchange (millions)",
        size = 10,
        rot = 90
      ),
      bottom = ggpubr::text_grob(
        "Year",
        size = 10
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}





# Current above here -----------------------------------------------------------




plot_bar_retention_region_season_length <- function (data,
                                                     plot_name,
                                                     size_text,
                                                     legend_name,
                                                     width = 190,
                                                     height = 150,
                                                     file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$previous_area == .data$current_area) %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area)) %>%
    dplyr::mutate(movement_time = factor(movement_time, levels = 1:4))


  # Assemble increase panel ----------------------------------------------------

  small_panel <- ggplot2::ggplot(
    data = data %>% dplyr::filter(released_group == 1),
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$movement_time
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Blues",
      type = "seq"
    ) +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Assemble decrease panel ----------------------------------------------------

  large_panel <- ggplot2::ggplot(
    data = data %>% dplyr::filter(released_group == 2),
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$movement_time
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Blues",
      type = "seq"
    ) +
    ggplot2::xlab("Region") +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Assemble panel figure ------------------------------------------------------

  p0 <- ggpubr::ggarrange(
    small_panel,
    large_panel,
    nrow = 2,
    labels = c("Small (400-549 mm)", "Large (550-800 mm)"),
    label.x = 0,
    label.y = 1,
    hjust = -0.7,
    vjust = 3,
    font.label = list(size = 8, color = "black", face = "plain"),
    legend = "right",
    common.legend = TRUE
  )

  ggpubr::annotate_figure(
    p0,
    left = ggpubr::text_grob(
      "Annual retention rate",
      size = size_text,
      rot = 90
    )
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_bar_region_season_pooled <- function (data,
                                           plot_name,
                                           size_text,
                                           legend_name,
                                           width = 190,
                                           height = 160,
                                           file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      previous_region_short = number_to_short(.data$previous_area),
      current_region_short = number_to_short(.data$current_area)
    ) %>%
    dplyr::mutate(movement_time = factor(movement_time, levels = 1:4))

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      # x = .data$current_region_short,
      x = .data$movement_time,
      y = .data$mean,
      fill = .data$movement_time
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      # position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2 #,
      # position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Blues",
      type = "seq"
    ) +
    ggplot2::facet_grid(
      rows = dplyr::vars(previous_region_short),
      cols = dplyr::vars(current_region_short),
      switch = "both"
    ) +
    # ggplot2::xlab("Quarter") +
    ggplot2::ylab("Annual retention rate") +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      position = "right"
    ) +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_bar_retention_region_season_pooled <- function (data,
                                                     plot_name,
                                                     size_text,
                                                     legend_name,
                                                     width = 190,
                                                     height = 80,
                                                     file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$previous_area == .data$current_area) %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area)) %>%
    dplyr::mutate(movement_time = factor(movement_time, levels = 1:4))

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$movement_time
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Blues",
      type = "seq"
    ) +
    ggplot2::xlab("Region") +
    ggplot2::ylab("Annual retention rate") +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_bar_sensitivity_harvest_priors <- function(study,
                                                sd_001,
                                                sd_005,
                                                sd_010,
                                                sd_015,
                                                sd_020,
                                                plot_name,
                                                size_text,
                                                legend_name,
                                                width = 190,
                                                height = 150,
                                                file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- dplyr::bind_rows(
    sd_001 %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "0.01"),
    sd_005 %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "0.05"),
    sd_010 %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "0.10"),
    sd_015 %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "0.15"),
    sd_020 %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "0.20"),
  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area))

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$id
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Blues",
      type = "seq"
    ) +
    ggplot2::xlab("Region") +
    ggplot2::ylab("Annual retention rate") +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_bar_sensitivity_reporting <- function (study,
                                            increase_ak,
                                            increase_bc,
                                            increase_cc,
                                            decrease_ak,
                                            decrease_bc,
                                            decrease_cc,
                                            plot_name,
                                            size_text,
                                            legend_name,
                                            width = 190,
                                            height = 150,
                                            file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  # Increase
  increase <- dplyr::bind_rows(
    study %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Study values"),
    increase_ak %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change AK"),
    increase_bc %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change BC"),
    increase_bc %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change CC")
  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area))

  # Decrease
  decrease <- dplyr::bind_rows(
    study %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Study values"),
    decrease_ak %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change AK"),
    decrease_bc %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change BC"),
    decrease_bc %>%
      dplyr::filter(.data$previous_area == .data$current_area) %>%
      dplyr::mutate(id = "Change CC")
  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area))

  # Assemble increase panel ----------------------------------------------------

  increase_panel <- ggplot2::ggplot(
    data = increase,
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$id
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      type = "div"
    ) +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Assemble decrease panel ----------------------------------------------------

  decrease_panel <- ggplot2::ggplot(
    data = decrease,
    mapping = ggplot2::aes(
      x = .data$region_short,
      y = .data$mean,
      fill = .data$id
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      color = "white"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(
      type = "div"
    ) +
    ggplot2::xlab("Region") +
    ggplot2::labs(fill = legend_name) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Assemble panel figure ------------------------------------------------------

  p0 <- ggpubr::ggarrange(
    increase_panel,
    decrease_panel,
    nrow = 2,
    labels = c("Increase 50%", "Decrease 33%"),
    label.x = 0,
    label.y = 1,
    hjust = -0.7,
    vjust = 3,
    font.label = list(size = 8, color = "black", face = "plain"),
    legend = "right",
    common.legend = TRUE
  )

  ggpubr::annotate_figure(
    p0,
    left = ggpubr::text_grob(
      "Annual retention rate",
      size = size_text,
      rot = 90
    )
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}


plot_heat_length <- function (data,
                              plot_name,
                              movement_time = 1,
                              released_group = c(1, 2),
                              size_text = 5,
                              xlab = NULL,
                              ylab = NULL,
                              xtext = TRUE,
                              ytext = TRUE,
                              margin_x = -15,
                              margin_y = -15,
                              font_size_mean = 3,
                              font_nudge_mean = 0.15,
                              font_size_sd = 2,
                              font_nudge_sd = 0.15,
                              legend_name = "Movement rate",
                              width = 190,
                              height = 110,
                              file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  small <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$movement_time == .env$movement_time[1],
      .data$released_group == .env$released_group[1],
    ) %>%
    dplyr::mutate(
      previous_short = number_to_short(.data$previous_area),
      current_short = number_to_short(.data$current_area)
    )

  large <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$movement_time == .env$movement_time[1],
      .data$released_group == .env$released_group[2],
    ) %>%
    dplyr::mutate(
      previous_short = number_to_short(.data$previous_area),
      current_short = number_to_short(.data$current_area)
    )

  # Construct small panel ------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = small,
    mapping = ggplot2::aes(
      x = .data$current_short,
      y = factor(
        .data$previous_short,
        levels = rev(levels(.data$previous_short))
      ),
      fill = .data$mean
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      width = 0.975,
      height = 0.975
    ) +
    # Use viridis
    ggplot2::scale_fill_viridis_c(
      direction = 1,
      option = "plasma",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25)
    ) +
    # Add mean
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = round_to_character(.data$mean, 2),
        col = as.factor(ifelse(.data$mean >= 0.5, 0, 1))
      ),
      fontface = "plain",
      nudge_y = font_nudge_mean,
      size = font_size_mean
    ) +
    # Add ci
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = paste0("(", round_to_character(.data$sd, 3), ")"),
        col = as.factor(ifelse(.data$mean >= 0.5, 0, 1))
      ),
      fontface = "plain",
      nudge_y = -font_nudge_sd,
      size = font_size_sd
    ) +
    # Text to black and white
    ggplot2::scale_color_grey(start = 0, end = 1) +
    ggplot2::guides(
      col = "none",
      fill = if (is.null(legend_name)) {
        "none"
      } else {
        ggplot2::guide_colorbar(
          title.position = "top",
          title.hjust = 0.5
        )
      }
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_name) +
    # Theme
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = size_text),
      legend.position = ifelse(is.null(legend_name), "none", "bottom"),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      legend.key.width = grid::unit(0.1, "npc"),
      legend.key.height = grid::unit(0.02, "npc"),
      legend.spacing.y = grid::unit(0.005, "npc"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 10, r = 1, b = 3, l = 2),
      axis.text.x = if (xtext) {
        ggplot2::element_text(margin = ggplot2::margin(t = margin_x))
      } else {
        NULL
      },
      axis.text.y = if (ytext) {
        ggplot2::element_text(margin = ggplot2::margin(r = margin_y))
      } else {
        NULL
      }
    )

  # Construct large panel ------------------------------------------------------

  p2 <- ggplot2::ggplot(
    data = large,
    mapping = ggplot2::aes(
      x = .data$current_short,
      y = factor(
        .data$previous_short,
        levels = rev(levels(.data$previous_short))
      ),
      fill = .data$mean
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      width = 0.975,
      height = 0.975
    ) +
    # Use viridis
    ggplot2::scale_fill_viridis_c(
      direction = 1,
      option = "plasma",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25)
    ) +
    # Add mean
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = round_to_character(.data$mean, 2),
        col = as.factor(ifelse(.data$mean >= 0.5, 0, 1))
      ),
      fontface = "plain",
      nudge_y = font_nudge_mean,
      size = font_size_mean
    ) +
    # Add ci
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        label = paste0("(", round_to_character(.data$sd, 3), ")"),
        col = as.factor(ifelse(.data$mean >= 0.5, 0, 1))
      ),
      fontface = "plain",
      nudge_y = -font_nudge_sd,
      size = font_size_sd
    ) +
    # Text to black and white
    ggplot2::scale_color_grey(start = 0, end = 1) +
    ggplot2::guides(
      col = "none",
      fill = if (is.null(legend_name)) {
        "none"
      } else {
        ggplot2::guide_colorbar(
          title.position = "top",
          title.hjust = 0.5
        )
      }
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_name) +
    # Theme
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = size_text),
      legend.position = ifelse(is.null(legend_name), "none", "bottom"),
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      legend.key.width = grid::unit(0.1, "npc"),
      legend.key.height = grid::unit(0.02, "npc"),
      legend.spacing.y = grid::unit(0.005, "npc"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 10, r = 1, b = 3, l = 2),
      axis.text.x = if (xtext) {
        ggplot2::element_text(margin = ggplot2::margin(t = margin_x))
      } else {
        NULL
      },
      axis.text.y = if (ytext) {
        ggplot2::element_text(margin = ggplot2::margin(r = margin_y))
      } else {
        NULL
      }
    )

  # Assemble panel figure ------------------------------------------------------

  ggpubr::ggarrange(
    p1,
    p2 + ggpubr::rremove("y.text"),
    labels = c("Small (400-549 mm)", "Large (550-800 mm)"),
    font.label = list(size = size_text, face = "plain"),
    label.x = 0,
    hjust = c(-0.23, -0.075),
    vjust = 1.7,
    ncol = 2,
    common.legend = TRUE,
    legend = "bottom"
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_point_region_year_pooled <- function (data,
                                           plot_name,
                                           size_line,
                                           size_point,
                                           size_text,
                                           year_offset,
                                           width = 190,
                                           height = 160,
                                           file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      previous_region_short = number_to_short(.data$previous_area),
      current_region_short = number_to_short(.data$current_area)
    ) %>%
    dplyr::mutate(year = movement_time + year_offset)

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$year,
      y = .data$mean
    )
  ) +
    ggplot2::geom_line(size = size_line) +
    ggplot2::geom_point(size = size_point) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2
    ) +
    ggplot2::coord_cartesian(xlim = c(1979, 2020)) +
    ggplot2::scale_x_continuous(
      breaks = c(1985, 2015),
      position = "top"
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      position = "right"
    ) +
    ggplot2::facet_grid(
      rows = dplyr::vars(previous_region_short),
      cols = dplyr::vars(current_region_short),
      switch = "both"
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Annual retention rate") +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_point_retention_region_year_length <- function (data,
                                                     plot_name,
                                                     size_line,
                                                     size_point,
                                                     size_text,
                                                     year_offset,
                                                     width = 190,
                                                     height = 150,
                                                     file_type = ".png") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$previous_area == .data$current_area) %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area)) %>%
    dplyr::mutate(year = movement_time + year_offset) %>%
    dplyr::mutate(
      released_length = ifelse(
        released_group == 1,
        "Small (400-549 mm)",
        "Large (550-800 mm)"
      )
    ) %>%
    dplyr::mutate(
      released_length = factor(
        released_length,
        levels = c("Small (400-549 mm)", "Large (550-800 mm)")
      )
    )

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$year,
      y = .data$mean
    )
  ) +
    ggplot2::geom_line(size = size_line) +
    ggplot2::geom_point(size = size_point) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2
    ) +
    ggplot2::coord_cartesian(xlim = c(1979, 2020)) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      position = "right"
    ) +
    ggplot2::facet_grid(
      rows = dplyr::vars(region_short),
      cols = dplyr::vars(released_length),
      switch = "y"
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Annual retention rate") +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_point_retention_region_year_pooled <- function (data,
                                                     plot_name,
                                                     size_line,
                                                     size_point,
                                                     size_text,
                                                     year_offset,
                                                     width = 90,
                                                     height = 150,
                                                     file_type = ".png") {
  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$previous_area == .data$current_area) %>%
    dplyr::mutate(region_short = number_to_short(.data$previous_area)) %>%
    dplyr::mutate(year = movement_time + year_offset)

  # Assemble figure ------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$year,
      y = .data$mean
    )
  ) +
    ggplot2::geom_line(size = size_line) +
    ggplot2::geom_point(size = size_point) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$mean - .data$sd,
        ymax = .data$mean + .data$sd
      ),
      width = 0.2
    ) +
    ggplot2::coord_cartesian(xlim = c(1979, 2020)) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      position = "right"
    ) +
    ggplot2::facet_grid(
      rows = dplyr::vars(region_short),
      switch = "y"
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Annual retention rate") +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size_text),
      axis.text.x = ggplot2::element_text(size = size_text),
      axis.title.y = ggplot2::element_text(size = size_text),
      axis.text.y = ggplot2::element_text(size = size_text),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = size_text),
      legend.text = ggplot2::element_text(size = size_text),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm"
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}
