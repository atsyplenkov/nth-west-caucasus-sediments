library(tidyverse)
library(data.table)
library(here)
library(scales)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

# Load data ----
list_df <-
  fs::dir_ls("data/images/", regexp = "*.csv$") |>
  as.list() |>
  lapply(fread)

list_df[[1]]$type <- "line"
list_df[[2]]$type <- "lower"
list_df[[4]]$type <- "upper"

dearing_jones <-
  list_df[c(1, 2, 4)] |>
  bind_rows() |>
  rename(Area = 1, SS = 2) |>
  pivot_wider(
    id_cols = Area,
    names_from = type,
    values_from = SS
  )

points_df <-
  list_df[[3]] |>
  rename(Area = 1, SS = 2) |>
  mutate(place = "Black Sea") |>
  add_row(
    Area = 45900,
    SS = 3.1,
    place = "Krasnodar Reservoir"
  )

# Plot ----
dd_formula <- "*y* = 9.55 &middot; *x*<sup>−0.19</sup>"

dd_plot <-
  ggplot() +
  geom_smooth(
    data = dearing_jones |>
      drop_na(line),
    aes(
      x = Area,
      y = line,
      color = "Line",
      lty = "Line",
      group = NULL
    ),
    se = FALSE,
    lwd = rel(1)
  ) +
  geom_smooth(
    data = dearing_jones |>
      drop_na(upper),
    aes(
      x = Area,
      y = upper,
      color = "CI",
      lty = "CI",
      group = NULL
    ),
    se = FALSE,
    lwd = rel(0.75),
  ) +
  geom_smooth(
    data = dearing_jones |>
      drop_na(lower),
    aes(
      x = Area,
      y = lower,
      color = "CI",
      lty = "CI",
      group = NULL
    ),
    lwd = rel(0.75),
    se = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = points_df,
    aes(
      x = Area,
      y = SS,
      label = place
    ),
    size = rel(3),
    nudge_y = 2.5,
    min.segment.length = 0,
    fontface = "plain",
    family = "Merriweather",
    segment.color = "dimgrey"
  ) +
  geom_point(
    data = points_df,
    aes(
      x = Area,
      y = SS
    ),
    fill = "white",
    shape = 21
  ) +
  scale_color_manual(
    name = "",
    labels = c("Prediction interval", dd_formula),
    values = c("grey40", "black")
  ) +
  scale_linetype_manual(
    name = "",
    labels = c("Prediction interval", dd_formula),
    values = c("84", "solid")
  ) +
  guides(linetype = guide_legend(override.aes = list(linetype = c("22", "solid")))) +
  scale_x_continuous(
    trans = "log10",
    breaks = trans_breaks("log10", n = 10, function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    expand = expansion(c(0, 0))
  ) +
  coord_cartesian(
    xlim = c(10^-1, 10^6.5),
    expand = FALSE,
  ) +
  labs(
    x = "**Drainage basin area [km<sup>2</sup>]**",
    y = "***S*<sub>max</sub> / *S*<sub>min</sub>**"
  ) +
  theme(
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    ),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.9),
    legend.justification = "left",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.spacing = unit(0.3, "lines"),
    legend.background = element_blank(),
    plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5.5)
  )

dd_plot

# Saved to SVG through httpgd
