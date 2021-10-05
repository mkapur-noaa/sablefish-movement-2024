#' Heat Map Matrix
#'
#' @param data [data.frame()]
#' @param movement_time [numeric()]
#' @param released_group [numeric()]
#' @param xlab [character()] or \code{NULL}
#' @param ylab [character()] or \code{NULL}
#' @param font_size_p [numeric()]
#' @param font_nudge_p [numeric()]
#' @param font_size_ci [numeric()]
#' @param font_nudge_ci [numeric()]
#' @param legend_name [character()] or \code{NULL}
#'
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @return [ggplot2::ggplot()]
#' @export
#'
heat_map_matrix <- function (data,
                             movement_time = 1,
                             released_group = 1,
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
                             legend_name = "Movement rate") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(
      .data$movement_time == .env$movement_time,
      .data$released_group == .env$released_group,
    ) %>%
    dplyr::mutate(
      previous_short = area_to_short(.data$previous_area),
      current_short = area_to_short(.data$current_area)
    )

  # Construct geom object ------------------------------------------------------

  ggplot2::ggplot(
    data = data,
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = ifelse(is.null(legend_name), "none", "bottom"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
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
}

#' Point Movement Plot
#'
#' @param data [data.frame()]
#' @param released_group [numeric()]
#'
#' @return [ggplot2::ggplot()]
#' @export
#'
point_movement <- function (data,
                            released_group = 1,
                            point_size = 0.8,
                            xlab = NULL,
                            ylab = NULL,
                            xlim = NULL,
                            ylim = NULL) {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(
      .data$released_group == .env$released_group,
    ) %>%
    dplyr::mutate(
      previous_short = area_to_short(.data$previous_area),
      current_short = area_to_short(.data$current_area),
      movement_label = time_to_label(.data$movement_time)
    )

  # Construct geom object ------------------------------------------------------

  ggplot2::ggplot(
    data = dplyr::filter(
      data,
      .data$released_group == .env$released_group
    ),
    mapping = ggplot2::aes(
      x = .data$movement_label,
      y = .data$mean
    )
  ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(ymin = q5, ymax = q95)
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$previous_short),
      cols = ggplot2::vars(.data$current_short),
      switch = "y"
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_y_continuous(position = "right") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white")
    )
}

#' Plot Heat Map Composite Figure
#'
#' @param data1 [data.frame()]
#' @param data2 [data.frame()]
#'
#' @return [character()] file path
#' @export
#'
plot_heat_composite <- function (data1, data2) {

  # Define plot name
  name <- "heat-composite"

  # Heat region average small
  p1 <- heat_map_matrix(
    data = data1,
    released_group = 1,
    margin_x = 0,
    margin_y = 0,
    legend_name = NULL
  )

  # Heat region average large
  p2 <- heat_map_matrix(
    data = data1,
    released_group = 2,
    ytext = FALSE,
    margin_x = 0,
    margin_y = 0,
    legend_name = NULL
  )

  # Heat region composite
  p3 <- ggpubr::ggarrange(
    p1, p2,
    ncol = 2
  )

  # Heat subregion average pooled
  p4 <- heat_map_matrix(
    data = data2,
    margin_x = 0,
    margin_y = 0,
    legend_name = "Movement rate"
  )

  # Heat composite
  p5 <- ggpubr::ggarrange(
    p3, p4,
    heights = c(1, 2),
    nrow = 2,
    common.legend = TRUE,
    legend = "bottom"
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))

  # Save ggplot
  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = 4,
    height = 6
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}
