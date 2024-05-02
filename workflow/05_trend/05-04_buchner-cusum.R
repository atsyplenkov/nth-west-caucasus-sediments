library(tidyverse)
library(here)
library(lubridate)
library(broom)
library(imputeTS)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

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

# Load data ---------------------------------------------------------------
buchner_areas <-
  qs::qread("workflow/05_trend/data/landcover_buchner2020.qs")

hyde_df <-
  qs::qread("workflow/05_trend/data/hyde_2023.qs")

glims_df <-
  qs::qread("workflow/05_trend/data/glims.qs")

wc_all <-
  qs::qread("workflow/05_trend/data/worldclim.qs") |>
  pluck(2) |>
  select(river, date, prec) |>
  group_by(river, Year = year(date)) |>
  reframe(
    Precipitation = sum(prec)
  )

# Extend the Cropland Area ------------------------------------------------
hyde_lm <-
  buchner_areas |>
  left_join(
    hyde_df |>
      select(river, Year = year, cropland),
    by = join_by(river, Year)
  ) %>%
  lm(CroplandArea ~ cropland, data = .)

buchner_hyde <-
  hyde_df |>
  select(river, Year = year, cropland) |>
  group_by(river) |>
  complete(Year = seq(1920, 2021, by = 1)) |>
  mutate(cropland = na_interpolation(cropland)) |>
  ungroup() |>
  left_join(
    buchner_areas,
    by = join_by(river, Year)
  ) |>
  augment(x = hyde_lm, data = .) |>
  filter(
    !is.na(CroplandArea) |
      Year %in% c(1960, 1965, 1970, 1975, 1980)
  ) |>
  mutate(
    Crops = ifelse(is.na(CroplandArea), .fitted, CroplandArea)
  )

# Quick CUSUM -------------------------------------------------------------
hyde_cusum <-
  hyde_df |>
  select(river, Year = year, cropland) |>
  group_by(river) |>
  complete(Year = seq(1920, 2021, by = 5)) |>
  mutate(cropland = na_interpolation(cropland)) |>
  ungroup() |>
  filter(Year >= 1987) |>
  gather(Type, Area, -river, -Year) |>
  group_by(river, Type) |>
  mutate(CDC = cumsum((Area - mean(Area)) / sd(Area))) |>
  ungroup()

buchner_cusum <-
  buchner_areas |>
  group_by(river) |>
  complete(Year = seq(1987, 2015, by = 2)) |>
  arrange(Year, .by_group = TRUE) |>
  mutate(
    across(
      c(CroplandArea, ForestArea),
      ~ na_interpolation(.x)
    )
  ) |>
  ungroup() |>
  gather(Type, Area, -river, -Year) |>
  group_by(river, Type) |>
  mutate(CDC = cumsum((Area - mean(Area)) / sd(Area))) |>
  ungroup()

worldclim_cusum <-
  wc_all |>
  filter(between(Year, 1970, 2021)) |>
  gather(Type, Var, -river, -Year) |>
  group_by(river, Type) |>
  mutate(CDC = cumsum((Var - mean(Var)) / sd(Var))) |>
  ungroup()

glims_cusum <- glims_df |>
  filter(GlimsArea != 0) |>
  group_by(river) |>
  complete(Year = seq(1960, 2020, by = 5)) |>
  arrange(Year, .by_group = TRUE) |>
  mutate(GlimsArea = na_interpolation(GlimsArea) / 10^6) |>
  filter(between(Year, 1970, 2020)) |>
  gather(Type, Var, -river, -Year) |>
  group_by(river, Type) |>
  # mutate(VarMean = mean(Var, na.rm = T),
  #        VarCv = sd(Var, na.rm = T) / VarMean) |>
  # mutate(K = Var / VarMean - 1) |>
  # mutate(CDC = cumsum(K) / VarCv) |>
  mutate(CDC = cumsum((Var - mean(Var)) / sd(Var))) |>
  ungroup()

buchner_hyde_cusum <-
  buchner_hyde |>
  select(river, Year, CroplandArea) |>
  gather(Type, Var, -river, -Year) |>
  group_by(river, Type) |>
  mutate(CDC = cumsum((Var - mean(Var)) / sd(Var))) |>
  ungroup()


glims_mean <-
  glims_cusum |>
  group_by(Year) |>
  ggdist::mean_qi(CDC) |>
  select(Year:.upper) |>
  mutate(
    river = "GLIMS",
    Type = "GlacierArea",
    .before = 1
  )

watershed_cusum_plot <-
  buchner_cusum |>
  bind_rows(worldclim_cusum) |>
  bind_rows(glims_mean) |>
  mutate(
    river = factor(
      river,
      levels = c(
        "Apchas", "Belaya", "Kuban", "Laba",
        "Marta", "Psekups", "Pshish", "Shunduk", "GLIMS"
      ),
      labels = c(
        "(a) Apchas", "(b) Belaya (No. 83361)",
        "(c) Kuban (No. 83174)", "(d) Laba (No. 83314)",
        "(e) Marta", "(f) Psekups", "(g) Pshish (No. 83387)",
        "(h) Shunduk", "(i) Belaya, Kuban and Laba average"
      )
    )
  ) |>
  ggplot(
    aes(
      x = Year,
      y = CDC,
      fill = Type,
      color = Type,
      group = Type
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    lty = "32"
  ) +
  geom_ribbon(
    aes(
      ymin = .lower,
      ymax = .upper
    ),
    fill = clrs[3],
    color = NA,
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_line(
    aes(lty = Type),
    lwd = rel(0.9),
    key_glyph = draw_key_timeseries
  ) +
  facet_wrap(~river, scales = "free") +
  labs(
    y = "**CUSUM**",
    x = "",
    color = "",
    fill = "",
    lty = ""
  ) +
  scale_color_manual(
    values = clrs[c(4, 5, 3, 7)],
    labels = c("Cropland", "Forest", "Glacier", "Precipitation")
  ) +
  scale_linetype_manual(
    values = c("solid", "32", "32", "solid"),
    labels = c("Cropland", "Forest", "Glacier", "Precipitation")
  ) +
  theme(
    legend.justification = "left",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.spacing = unit(0.3, "lines"),
    legend.background = element_blank(),
    plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5.5)
  ) +
  theme_kbn()

watershed_cusum_plot

# Saved to SVG through httpgd

# Trend analysis -----------------------------------------------------------
library(trend)

zonal_pt <-
  buchner_cusum |>
  rename(Var = Area) |>
  bind_rows(worldclim_cusum) |>
  bind_rows(glims_cusum) |>
  group_by(river, Type) |>
  nest() |>
  mutate(pt = map(
    data,
    ~ trend::pettitt.test(.x$Var)$estimate
  )) |>
  mutate(p = map(
    data,
    ~ trend::pettitt.test(.x$Var)$p.value
  )) |>
  mutate(years = map(
    data, ~ sort(.x$Year)
  )) |>
  mutate(
    m = map_dbl(
      data, ~ mean(.x$Var)
    )
  ) |>
  unnest(cols = c(pt, p)) |>
  select(-data) |>
  ungroup() |>
  mutate(
    break_year = map2_dbl(years, pt, ~ .x[.y])
  ) |>
  select(-years)

zonal_pt |>
  filter(Type == "Precipitation") |>
  pull(m) |>
  mean()

# Taylor -----------------------------------------------------------
library(ChangePointTaylor)

set.seed(1234)
taylor_res <-
  buchner_cusum |>
  rename(Var = Area) |>
  group_by(river, Type) |>
  nest() |>
  mutate(taylor = map(
    data,
    ~ change_point_analyzer(
      x = .x$Var,
      labels = .x$Year,
      # min_candidate_conf = 0.90,
      # min_tbl_conf = 0.95,
      n_bootstraps = 5000,
      CI = 0.95
    )
  )) |>
  dplyr::select(-data) |>
  unnest(cols = c(taylor)) |>
  ungroup()

taylor_ci <-
  taylor_res |>
  mutate(
    label_low = str_split(`CI (95%)`, "-", simplify = TRUE)[, 1],
    label_upper = str_split(`CI (95%)`, "-", simplify = TRUE)[, 2],
  ) |>
  mutate(across(contains("label_"), ~ parse_number(.x)))

taylor_ci |>
  filter(Type == "CroplandArea") |>
  count(label, sort = TRUE)

# Timeseries -----------------------------------------------------------
buchner_cusum |>
  filter(Type == "ForestArea") |>
  left_join(ws_area, by = join_by(river)) |>
  transmute(river, Year, Type, Area = Area / area) |>
  ggplot(
    aes(Year, Area, color = Type)
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~river, scales = "free_y")

# GLIMS -------------------------------------------------------------------
glims_cusum <- glims_df |>
  filter(GlimsArea != 0) |>
  gather(Type, Var, -river, -Year) |>
  group_by(river, Type) |>
  mutate(
    VarMean = mean(Var, na.rm = TRUE),
    VarSD = sd(Var, na.rm = TRUE)
  ) |>
  mutate(
    z = (Var - VarMean) / VarSD,
    CDC = cumsum(z)
  ) |>
  # mutate(K = (Var / VarMean) - 1) |>
  # mutate(CDC = cumsum(K) / VarCv) |>
  ungroup()

glims_cusum |>
  ggplot(
    aes(
      x = Year,
      y = CDC
    )
  ) +
  geom_line() +
  facet_wrap(~river, scales = "free_y")

buchner_areas |>
  left_join(
    hyde_df |>
      select(river, Year = year, pasture, cropland, grazing),
    by = join_by(river, Year)
  ) |>
  mutate(CroplandHYDE = cropland) |>
  ggplot(
    aes(
      x = CroplandHYDE,
      y = CroplandArea
    )
  ) +
  geom_point() +
  geom_abline() +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq(
    ggpmisc::use_label(c("eq", "R2"))
  ) +
  scale_x_log10() +
  scale_y_log10()

# Prec Trends -------------------------------------------------------------
wc_all |>
  filter(river == "Pshish") |>
  mutate(Period = santoku::chop(Year, 1986, santoku::lbl_dash())) |>
  ggplot(
    aes(
      x = Year,
      y = Precipitation,
      color = Period
    )
  ) +
  geom_point() +
  geom_smooth(
    aes(
      x = Year,
      y = Precipitation
    ),
    method = "lm",
    inherit.aes = FALSE
  )

library(ChangePointTaylor)
set.seed(1234)
# taylor_res <-
wc_all |>
  filter(river == "Pshish") |>
  group_by(river) |>
  nest() |>
  mutate(taylor = map(
    data,
    ~ change_point_analyzer(
      x = .x$Precipitation,
      labels = .x$Year,
      min_candidate_conf = 0.3,
      min_tbl_conf = 0.5,
      n_bootstraps = 1000
      # CI = 0.95
      # method = "CUSUM"
    )
  )) |>
  dplyr::select(-data) |>
  unnest(cols = c(taylor)) |>
  ungroup()
