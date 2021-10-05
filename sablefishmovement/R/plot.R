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
    ggplot2::theme_void() +
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

#' Presentation Point Movement Plot
#'
#' @param data [data.frame()]
#' @param released_group [numeric()]
#'
#' @return [ggplot2::ggplot()]
#' @export
#'
pres_point_movement <- function (data,
                                 name,
                                 released_group = 1,
                                 point_size = 0.8,
                                 xlab = NULL,
                                 ylab = NULL,
                                 xlim = NULL,
                                 ylim = NULL,
                                 width = 4,
                                 height = 4) {

  # Construct geom object ------------------------------------------------------

  p1 <- point_movement(data = data,
                       released_group = released_group,
                       point_size = point_size,
                       xlab = xlab,
                       ylab = ylab,
                       xlim = xlim,
                       ylim = ylim)

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
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

#' Presentation Heat Pooled
#'
#' @param data [data.frame()]
#' @param name [character()]
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
pres_heat_pooled <- function (data,
                              name,
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
                              legend_name = "Movement rate",
                              width = 3,
                              height = 3) {


  # Construct geom object ------------------------------------------------------

  p1 <- heat_map_matrix(data = data,
                        movement_time = movement_time,
                        released_group = released_group,
                        xlab = xlab,
                        ylab = ylab,
                        xtext = xtext,
                        ytext = ytext,
                        margin_x = margin_x,
                        margin_y = margin_y,
                        font_size_mean = font_size_mean,
                        font_nudge_mean = font_nudge_mean,
                        font_size_sd = font_size_sd,
                        font_nudge_sd = font_nudge_sd,
                        legend_name = legend_name)  +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}

#' Presentation Heat Length
#'
#' @param data [data.frame()]
#' @param name [character()]
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
pres_heat_length <- function (data,
                              name,
                              movement_time = 1,
                              released_group = c(1, 2),
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
                              width = 5,
                              height = 3) {

  # Construct geom objects -----------------------------------------------------

  p1 <- heat_map_matrix(data = data,
                        movement_time = movement_time,
                        released_group = released_group[1],
                        xlab = xlab,
                        ylab = ylab,
                        xtext = xtext,
                        ytext = ytext,
                        margin_x = margin_x,
                        margin_y = margin_y,
                        font_size_mean = font_size_mean,
                        font_nudge_mean = font_nudge_mean,
                        font_size_sd = font_size_sd,
                        font_nudge_sd = font_nudge_sd,
                        legend_name = legend_name)

  p2 <- heat_map_matrix(data = data,
                        movement_time = movement_time,
                        released_group = released_group[2],
                        xlab = xlab,
                        ylab = ylab,
                        xtext = xtext,
                        ytext = FALSE,
                        margin_x = margin_x,
                        margin_y = margin_y,
                        font_size_mean = font_size_mean,
                        font_nudge_mean = font_nudge_mean,
                        font_size_sd = font_size_sd,
                        font_nudge_sd = font_nudge_sd,
                        legend_name = legend_name)

  p3 <- ggpubr::ggarrange(
    p1, p2,
    ncol = 2,
    common.legend = TRUE,
    legend = "bottom"
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}

#' Presentation Bar Quarter
#'
#' @param data [data.frame()]
#' @param name [character()]
#' @param released_group [numeric()]
#' @param xlab [character()]
#' @param ylab [character()]
#' @param ylim [numeric()]
#' @param xtext [logical()]
#' @param ytext [logical()]
#' @param width [numeric()]
#' @param height [numeric()]
#' @param legend_name [character()]
#'
#' @return [character()] file path
#' @export
#'
pres_bar_quarter <- function (data,
                              name,
                              released_group = 1,
                              xlab = "Quarter",
                              ylab = "Movement rate",
                              ylim = c(0, 1),
                              xtext = TRUE,
                              ytext = TRUE,
                              width = 5,
                              height = 3,
                              legend_name = "Region") {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(
      .data$released_group %in% .env$released_group,
      .data$previous_area == .data$current_area
    ) %>%
    dplyr::mutate(
      short = area_to_short(.data$current_area),
      movement_label = time_to_label(.data$movement_time)
    )

  # Prepare geom objects -------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = dplyr::filter(data, .data$released_group == .env$released_group[1]),
    mapping = ggplot2::aes(
      x = .data$movement_label,
      y = .data$mean,
      fill = .data$short
    )
  ) +
    ggplot2::scale_fill_brewer(
      type = "qual",
      palette = "Dark2",
      direction = -1
    ) +
    ggplot2::geom_bar(
      width = 0.75,
      position = ggplot2::position_dodge(0.8),
      stat = "identity"
    ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(ymin = q5, ymax = q95),
      position = ggplot2::position_dodge(0.8)
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_name) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (length(released_group) == 2) {
    p2 <- ggplot2::ggplot(
      data = dplyr::filter(data, .data$released_group == .env$released_group[2]),
      mapping = ggplot2::aes(
        x = .data$movement_label,
        y = .data$mean,
        fill = .data$short
      )
    ) +
      ggplot2::scale_fill_brewer(
        type = "qual",
        palette = "Dark2",
        direction = -1
      ) +
      ggplot2::geom_bar(
        width = 0.75,
        position = ggplot2::position_dodge(0.8),
        stat = "identity"
      ) +
      ggplot2::geom_linerange(
        mapping = ggplot2::aes(ymin = q5, ymax = q95),
        position = ggplot2::position_dodge(0.8)
      ) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(fill = legend_name) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    p3 <- ggpubr::ggarrange(
      p1, p2,
      nrow = 2,
      legend = "right",
      common.legend = TRUE
    ) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  } else {
    p4 <- p1  +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  }

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}

#' Presentation Harvest Rates
#'
#' @param data [data.frame()]
#' @param name [character()]
#' @param harvest_group [numeric()]
#' @param xlab [character()]
#' @param ylab [character()]
#' @param xlim [numeric()]
#' @param ylim [numeric()]
#' @param point_size [numeric()]
#' @param width [numeric()]
#' @param height [numeric()]
#'
#' @return
#' @export
#'
pres_harvest <- function (data,
                          name,
                          harvest_group = 1,
                          xlab = "Year",
                          ylab = "Harvest rate",
                          xlim = c(1979, 2020),
                          ylim = c(0, 0.2),
                          point_size = 0.8,
                          width = 5,
                          height = 4) {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(
      .data$harvest_group %in% .env$harvest_group,
    ) %>%
    dplyr::mutate(
      short = area_to_short(.data$current_area),
      harvest_label = time_to_label(.data$harvest_time)
    )

  # Prepare geom object --------------------------------------------------------

  p1 <- ggplot2::ggplot(data = data) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        ymin = .data$prior_mean - 1.645 * .data$prior_sd,
        ymax = .data$prior_mean + 1.645 * .data$prior_sd
      ),
      fill = "lightgrey"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        y = .data$prior_mean
      ),
      color = "darkgrey"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        y = .data$mean
      ),
      size = point_size
    ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        ymin = .data$q5,
        ymax = .data$q95
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$short),
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

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}


#' Presentation Harvest Rates By Length
#'
#' @param data [data.frame()]
#' @param name [character()]
#' @param harvest_group [numeric()]
#' @param xlab [character()]
#' @param ylab [character()]
#' @param xlim [numeric()]
#' @param ylim [numeric()]
#' @param point_size [numeric()]
#' @param width [numeric()]
#' @param height [numeric()]
#'
#' @return
#' @export
#'
pres_harvest_length <- function (data,
                                 name,
                                 length_class = c("Small", "Large"),
                                 harvest_group = c(1, 2),
                                 xlab = "Year",
                                 ylab = "Harvest rate",
                                 xlim = c(1979, 2020),
                                 ylim = c(0, 0.2),
                                 point_size = 0.8,
                                 width = 5,
                                 height = 4) {

  # Prepare data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(
      .data$harvest_group %in% .env$harvest_group,
    ) %>%
    dplyr::mutate(
      short = area_to_short(.data$current_area),
      harvest_label = time_to_label(.data$harvest_time),
      harvest_group_label = factor(
        length_class[.data$harvest_group],
        levels = length_class
      )
    )

  # Prepare geom object --------------------------------------------------------

  p1 <- ggplot2::ggplot(data = data) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        ymin = .data$prior_mean - 1.645 * .data$prior_sd,
        ymax = .data$prior_mean + 1.645 * .data$prior_sd
      ),
      fill = "lightgrey"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        y = .data$prior_mean
      ),
      color = "darkgrey"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        y = .data$mean
      ),
      size = point_size
    ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(
        x = .data$harvest_label,
        ymin = .data$q5,
        ymax = .data$q95
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$short),
      cols = ggplot2::vars(.data$harvest_group_label),
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

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("manuscript", "figs", paste0(name, ".png")),
    width = width,
    height = height
  )

  # Return path
  return(paste0("manuscript/", "figs/", name, ".png"))
}
