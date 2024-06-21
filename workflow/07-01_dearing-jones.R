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
ggplot() +
  geom_smooth(
    data = dearing_jones |>
      drop_na(line),
    aes(
      x = Area,
      y = line,
      color = "Line",
      group = NULL
    ),
    se = FALSE,
    lwd = 1.5
  ) +
  geom_smooth(
    data = dearing_jones |>
      drop_na(upper),
    aes(
      x = Area,
      y = upper,
      color = "CI",
      group = NULL
    ),
    se = FALSE
  ) +
  geom_smooth(
    data = dearing_jones |>
      drop_na(lower),
    aes(
      x = Area,
      y = lower,
      color = "CI",
      group = NULL
    ),
    se = FALSE
  ) +
  geom_point(
    data = points_df,
    aes(
      x = Area,
      y = SS
    )
  ) +
  ggrepel::geom_text_repel(
    data = points_df,
    aes(
      x = Area,
      y = SS,
      label = place
    ),
    nudge_y = 2.5,
    min.segment.length = 0,
    fontface = "bold",
    family = "Merriweather"
  ) +
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
    x = "Drainage basin area [km<sup>2</sup>]",
    y = "*S*<sub>max</sub> / *S*<sub>min</sub>"
  )
