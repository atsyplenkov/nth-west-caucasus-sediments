library(here)
library(tidyverse)
library(ggnewscale)
library(trend)
library(ChangePointTaylor)
library(geomtextpath)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())
theme_replace(
  axis.title.y = element_text(
    size = rel(0.9),
    angle = 90
  )
)

clrs <- MetBrewer::met.brewer("Johnson", n = 8)
clrs[3] <- "grey10"

# Load watersheds ---------------------------------------------------------
ws_sf <-
  sf::st_read(
    "data/vector/kbn_ws-predict/kbn_ws-predict.shp"
  )

ws_area <-
  data.frame(
    river = ws_sf$river,
    area = as.numeric(sf::st_area(ws_sf)) / 10^6
  )

# load HYDE data ----------------------------------------------------------
hyde_df <-
  qs::qread("workflow/05_trend/data/hyde_2023.qs") |>
  as_tibble() |>
  relocate(year, river, everything())

hyde_area <-
  hyde_df |>
  left_join(ws_area, by = join_by(river)) |>
  mutate(
    across(c(conv_rangeland:rangeland), ~ {
      .x / area
    })
  )

hyde_area |>
  select(!contains("rice")) |>
  select(!contains("rainfed")) |>
  gather(
    type, type_area,
    -river, -year, -area
  ) |>
  group_by(river, type) |>
  reframe(
    area_mean = mean(type_area, na.rm = TRUE)
  ) |>
  ggplot(
    aes(
      x = type,
      y = area_mean,
      fill = type
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(area_mean, accuracy = 0.01))
  ) +
  facet_wrap(~river, scales = "free_y")

# CUSUM -------------------------------------------------------------------
hyde_long <-
  hyde_df |>
  group_by(river) |>
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = TRUE) |>
  mutate(Flag = ifelse(is.na(cropland), "sim", "obs")) |>
  mutate(across(
    where(is.double),
    ~ imputeTS::na_interpolation(.x, "linear")
  )) |>
  ungroup() |>
  filter(year >= 1925) |>
  gather(
    type, area,
    -river, -year, -Flag
  )

hyde_cusum <-
  hyde_long |>
  group_by(river, type) |>
  mutate(
    AreaMean = mean(area, na.rm = TRUE),
    AreaCv = sd(area, na.rm = TRUE) / AreaMean
  ) |>
  mutate(K = area / AreaMean - 1) |>
  mutate(CDC = cumsum(K) / AreaCv) |>
  ungroup()

hyde_cusum |>
  filter(type %in% c("pasture")) |>
  ggplot(
    aes(
      x = year,
      y = CDC,
      group = type,
      color = type
    )
  ) +
  geom_line() +
  geom_point(
    data = . %>% filter(Flag == "obs"),
    aes(
      x = year,
      y = CDC,
      color = type
    )
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    limits = c(1925, 2021),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  facet_wrap(
    ~river,
    scales = "free_y"
  ) +
  labs(
    x = "",
    y = "CUSUM Pasture"
  ) +
  guides(
    fill = guide_legend(
      order = 3,
      ncol = 1,
      title.position = "top"
    )
  )
