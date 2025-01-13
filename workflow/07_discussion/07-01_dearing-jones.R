library(tidyverse)
library(data.table)
library(here)
library(scales)

source(here::here("R", "funs_ggplot2.R"))

ggplot2::theme_set(theme_kbn())

# Load data ----
list_df <- fs::dir_ls("data/images/", regexp = "*.csv$") |>
  as.list() |>
  lapply(fread)

list_df[[1]]$type <- "line"
list_df[[2]]$type <- "lower"
list_df[[4]]$type <- "upper"

dearing_jones <- list_df[c(1, 2, 4)] |>
  dplyr::bind_rows() |>
  dplyr::rename(Area = 1, SS = 2) |>
  tidyr::pivot_wider(
    id_cols = Area,
    names_from = type,
    values_from = SS
  )

points_df <- list_df[[3]] |>
  dplyr::rename(Area = 1, SS = 2) |>
  dplyr::mutate(place = "Black Sea") |>
  dplyr::add_row(
    Area = 45900,
    SS = 3.1,
    place = "Krasnodar Reservoir"
  )

# Plot ----
dd_formula <- "*y* = 9.55 &middot; *x*<sup>−0.19</sup>"

dd_plot <- ggplot2::ggplot() +
  ggplot2::geom_smooth(
    data = dearing_jones |>
      tidyr::drop_na(line),
    ggplot2::aes(
      x = Area,
      y = line,
      color = "Line",
      lty = "Line",
      group = NULL
    ),
    se = FALSE,
    lwd = ggplot2::rel(1)
  ) +
  ggplot2::geom_smooth(
    data = dearing_jones |>
      tidyr::drop_na(upper),
    ggplot2::aes(
      x = Area,
      y = upper,
      color = "CI",
      lty = "CI",
      group = NULL
    ),
    se = FALSE,
    lwd = ggplot2::rel(0.75),
  ) +
  ggplot2::geom_smooth(
    data = dearing_jones |>
      tidyr::drop_na(lower),
    ggplot2::aes(
      x = Area,
      y = lower,
      color = "CI",
      lty = "CI",
      group = NULL
    ),
    lwd = ggplot2::rel(0.75),
    se = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = points_df,
    ggplot2::aes(
      x = Area,
      y = SS,
      label = place
    ),
    size = ggplot2::rel(3),
    nudge_y = 2.5,
    min.segment.length = 0,
    fontface = "plain",
    family = "Merriweather",
    segment.color = "dimgrey"
  ) +
  ggplot2::geom_point(
    data = points_df,
    ggplot2::aes(
      x = Area,
      y = SS
    ),
    fill = "white",
    shape = 21
  ) +
  ggplot2::scale_color_manual(
    name = "",
    labels = c("Prediction interval", dd_formula),
    values = c("grey40", "black")
  ) +
  ggplot2::scale_linetype_manual(
    name = "",
    labels = c("Prediction interval", dd_formula),
    values = c("84", "solid")
  ) +
  ggplot2::guides(
    linetype = ggplot2::guide_legend(
      override.aes = list(linetype = c("22", "solid"))
    )
  ) +
  ggplot2::scale_x_continuous(
    trans = "log10",
    breaks = scales::trans_breaks("log10", n = 10, function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 40),
    expand = ggplot2::expansion(c(0, 0))
  ) +
  ggplot2::coord_cartesian(
    xlim = c(10^-1, 10^6.5),
    expand = FALSE,
  ) +
  ggplot2::labs(
    x = "**Drainage basin area [km<sup>2</sup>]**",
    y = "***S*<sub>max</sub> / *S*<sub>min</sub>**"
  ) +
  ggplot2::theme(
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    ),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.9),
    legend.justification = "left",
    legend.key.height = ggplot2::unit(0.7, "lines"),
    legend.key.width = ggplot2::unit(1.5, "lines"),
    legend.spacing = ggplot2::unit(0.3, "lines"),
    legend.background = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 5.5, r = 12, b = 5.5, l = 5.5)
  )

dd_plot

# Saved to SVG through httpgd
