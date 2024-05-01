library(tidyverse)
library(sf)
library(here)
library(rmapshaper)
library(terra)
library(exactextractr)
library(emmeans)
library(imputeTS)
library(santoku)
library(sjPlot)

source("R/funs_ggplot2.R")
theme_set(theme_kbn())

clrs <- MetBrewer::met.brewer("Johnson", n = 8)
clrs[3] <- "grey10"

# Sediment data
sed_data <-
  qs::qread(
    here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs")
  ) |>
  mutate(id = as.character(id)) |>
  select(id:ssd_mean)

# Point data ---------------------------------------------------------
kbn_gages <-
  st_read(
    here("data", "vector", "kbn_gages", "kbn_gages.shp"),
    query = "SELECT * FROM kbn_gages WHERE region = 'nw'"
  ) |>
  mutate(id = as.character(id))

# Keep only "pristine" gages
rtop_ids <-
  kbn_gages |>
  filter(is.na(status2)) |>
  pull(id)

north_ids <-
  kbn_gages |>
  pull(id)

# 2) Watersheds -----------------------------------------------------------
ws_all <-
  st_read(
    here("data/vector/kbn_ws_30dec/kbn_ws_30dec.shp"),
    query = "SELECT id, area FROM kbn_ws_30dec"
  ) |>
  mutate(
    id = as.character(id)
  ) |>
  filter(id %in% north_ids) |>
  ms_simplify(keep = 0.3)

# Observed data
ws_obs <-
  ws_all |>
  filter(id %in% rtop_ids)

# Raster paths ------------------------------------------------------------
global_path <-
  ifelse(.Platform$OS.type == "unix", "/mnt/e/WORK/00_GLOBAL/", "E:/WORK/00_GLOBAL/")

# WorldClim 2.1
wc_parent_path <-
  paste0(global_path, "WorldClim/histroical/5min/")

prec_files <-
  fs::dir_ls(paste0(wc_parent_path, "/prec"))

# Forest change in 1987-2015
# Buchner et al., 2020
forest_file <-
  paste0(global_path, "Caucasus landuse/Caucasus_forest_change_map_1987-2015.tif")

# Cropland change in 1987-2015
# Buchner et al., 2020
cropland_file <-
  paste0(global_path, "Caucasus landuse/Caucasus_cropland_change_map_1987-2015.tif")

# Abandond areas
aal_path <-
  paste0(global_path, "hybrid_map_of_abandoned_arable_land_in_fSU/hybrid map of abandoned arable land.tif")

# Abandoned arable lands  -------------------------------------------------
aal_rast <-
  rast(aal_path)

aal_freqs <-
  exact_extract(
    aal_rast, st_transform(ws_obs, 4326),
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    },
    append_cols = c("id", "area")
  )

aal_tidy <-
  aal_freqs |>
  mutate(type = case_when(
    value == 1 ~ "Cropland",
    value == 2 ~ "AAL",
    TRUE ~ "Other"
  ))

aal_long <-
  aal_tidy |>
  # filter(value != 3) |>
  select(id, type, freq) |>
  pivot_wider(
    id_cols = c(id),
    values_from = c(freq),
    names_from = type
  ) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
  mutate(TotAban = AAL / (Cropland + AAL)) |>
  mutate(across(where(is.numeric), ~ replace(.x, is.nan(.x), 0)))


# WorldClim ---------------------------------------------------------------
prec_rast <- rast(prec_files)

prec_mean <-
  exact_extract(
    prec_rast,
    st_transform(ws_obs, 4326),
    "mean"
  )

# Prec
prec_tidy <-
  prec_mean |>
  mutate(river = ws_obs$id, .before = 1) |>
  gather(date, prec, -river) |>
  as_tibble() |>
  mutate(
    date = str_remove_all(date, "mean.wc2.1_5m_prec_|.tif"),
    date = lubridate::as_date(paste0(date, "-01"))
  )

prec_year <-
  prec_tidy |>
  group_by(river, Year = year(date)) |>
  reframe(
    prec = sum(prec)
  )

# Forest ------------------------------------------------------------------
# Read forest raster
forest_rast <-
  rast(forest_file)

ws_david <-
  ws_obs |>
  st_transform(crs = crs(forest_rast))

# Extract forest values
forest_freqs <-
  exact_extract(
    forest_rast, ws_david,
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    },
    append_cols = c("id", "area")
  )

# Tidy up forest extraction
forest_freqs_tidy <-
  forest_freqs |>
  mutate(type = case_when(
    value == 0 ~ "no forest",
    value == 1 ~ "gain 2015",
    value == 2 ~ "transitional forest",
    value == 11 ~ "gain 2010",
    value == 111 ~ "gain 2005",
    value == 1111 ~ "gain 2000",
    value == 9999 ~ "no data",
    value == 11111 ~ "gain 1995",
    value == 100000 ~ "loss 1995",
    value == 110000 ~ "loss 2000",
    value == 111000 ~ "loss 2005",
    value == 111100 ~ "loss 2010",
    value == 111110 ~ "loss 2015",
    value == 111111 ~ "stable forest",
    is.na(value) ~ "no data"
  )) |>
  select(id, area, type, freq) |>
  mutate(area = freq * area)

# Get forest areas for every year
forest_tidy <-
  forest_freqs_tidy |>
  filter(!type %in% c("no data", "no forest")) |>
  dplyr::select(id, area, type) |>
  group_by(id) |>
  nest() |>
  mutate(f1987 = map_dbl(
    data,
    ~ (.x$area[.x$type == "stable forest"] +
      .x$area[.x$type == "transitional forest"])
  )) |>
  mutate(f1995 = map2_dbl(
    data, f1987,
    ~ (.y + .x$area[.x$type == "gain 1995"] -
      .x$area[.x$type == "loss 1995"])
  )) |>
  mutate(f2000 = map2_dbl(
    data, f1995,
    ~ (.y + .x$area[.x$type == "gain 2000"] -
      .x$area[.x$type == "loss 2000"])
  )) |>
  mutate(f2005 = map2_dbl(
    data, f2000,
    ~ (.y + .x$area[.x$type == "gain 2005"] -
      .x$area[.x$type == "loss 2005"])
  )) |>
  mutate(f2010 = map2_dbl(
    data, f2005,
    ~ (.y + .x$area[.x$type == "gain 2010"] -
      .x$area[.x$type == "loss 2010"])
  )) |>
  mutate(f2015 = map2_dbl(
    data, f2010,
    ~ (.y + .x$area[.x$type == "gain 2015"] -
      .x$area[.x$type == "loss 2015"])
  )) |>
  dplyr::select(-data) |>
  ungroup() |>
  pivot_longer(
    names_to = "Year",
    values_to = "ForestArea",
    cols = contains("f")
  ) |>
  mutate(Year = parse_integer(str_remove(Year, "f")))

# Cropland ----------------------------------------------------------------
# Read cropland raster
cropland_rast <-
  rast(cropland_file)

# Extract forest values
cropland_freqs <-
  exact_extract(
    cropland_rast, ws_david,
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    },
    append_cols = c("id", "area")
  )

# Tidy up forest extraction
cropland_freqs_tidy <-
  cropland_freqs |>
  mutate(type = case_when(
    value == 0 ~ "no cropland",
    value == 1 ~ "gain 2015",
    value == 2 ~ "transitional cropland",
    value == 11 ~ "gain 2010",
    value == 111 ~ "gain 2005",
    value == 1111 ~ "gain 2000",
    value == 9999 ~ "no data",
    value == 11111 ~ "gain 1995",
    value == 100000 ~ "loss 1995",
    value == 110000 ~ "loss 2000",
    value == 111000 ~ "loss 2005",
    value == 111100 ~ "loss 2010",
    value == 111110 ~ "loss 2015",
    value == 111111 ~ "stable cropland",
    is.na(value) ~ "no data"
  )) |>
  select(id, area, type, freq) |>
  mutate(area = freq * area) |>
  complete(id, type)

# Get forest areas for every year
cropland_tidy <-
  cropland_freqs_tidy |>
  filter(!type %in% c("no data", "no cropland")) |>
  dplyr::select(id, area, type) |>
  group_by(id) %>%
  nest() %>%
  mutate(c1987 = map_dbl(
    data,
    ~ (.x$area[.x$type == "stable cropland"] +
      .x$area[.x$type == "transitional cropland"])
  )) %>%
  mutate(c1995 = map2_dbl(
    data, c1987,
    ~ (.y + .x$area[.x$type == "gain 1995"] -
      .x$area[.x$type == "loss 1995"])
  )) %>%
  mutate(c2000 = map2_dbl(
    data, c1995,
    ~ (.y + .x$area[.x$type == "gain 2000"] -
      .x$area[.x$type == "loss 2000"])
  )) %>%
  mutate(c2005 = map2_dbl(
    data, c2000,
    ~ (.y + .x$area[.x$type == "gain 2005"] -
      .x$area[.x$type == "loss 2005"])
  )) %>%
  mutate(c2010 = map2_dbl(
    data, c2005,
    ~ (.y + .x$area[.x$type == "gain 2010"] -
      .x$area[.x$type == "loss 2010"])
  )) %>%
  mutate(c2015 = map2_dbl(
    data, c2010,
    ~ (.y + .x$area[.x$type == "gain 2015"] -
      .x$area[.x$type == "loss 2015"])
  )) %>%
  dplyr::select(-data) %>%
  ungroup() |>
  pivot_longer(
    names_to = "Year",
    values_to = "CroplandArea",
    cols = contains("c")
  ) |>
  mutate(Year = parse_integer(str_remove(Year, "c")))

# Merge together ----------------------------------------------------------
buchner_areas <-
  left_join(
    cropland_tidy,
    forest_tidy,
    by = join_by(id, Year)
  )

# Save
# qs::qsave(
#   buchner_areas,
#   "workflow/06_all-rivers-stats/data/all_landcover_buchner2020.qs"
# )

# GLIMS -------------------------------------------------------------------
library(geos)

ws_geos <-
  ws_obs |>
  as_geos_geometry()

glims_path <-
  "data/vector/glims.gpkg"

glims_sf <-
  st_read(glims_path) |>
  st_zm() |>
  transmute(glac_id, year = as.POSIXct(src_date, tz = "UTC") |>
    year()) |>
  st_transform(st_crs(ws_obs)) |>
  ms_explode() |>
  mutate(
    year2 = case_when(
      year == 1960 ~ 1960,
      between(year, 1985, 1987) ~ 1986,
      between(year, 1999, 2001) ~ 2000,
      between(year, 2013, 2014) ~ 2014,
      TRUE ~ year
    )
  )

glims_tidy <-
  glims_sf |>
  mutate(area = as.numeric(st_area(glims_sf))) |>
  group_by(year2, glac_id) |>
  filter(
    area == max(area)
  ) |>
  ungroup()

get_glims_year <-
  function(.yr) {
    glims_year <-
      subset(glims_tidy, glims_tidy$year2 == .yr) |>
      as_geos_geometry()

    glims_area <- vector(mode = "numeric", length = 8L)

    for (i in seq_along(ws_geos)) {
      glims_area[i] <-
        glims_year[geos_prepared_intersects(glims_year, ws_geos[i])] |>
        geos_area() |>
        sum()
    }

    data.frame(
      Year = .yr,
      id = ws_obs$id,
      Area = ws_obs$area,
      GlimsArea = glims_area
    )
  }

glims_df <-
  lapply(
    sort(unique(glims_tidy$year2)),
    get_glims_year
  ) |>
  bind_rows()

glims_complete <-
  glims_df |>
  group_by(id, Area) |>
  complete(Year = seq(1960, 2021, by = 1)) |>
  mutate(across(GlimsArea, ~ na_interpolation(.x))) |>
  mutate(across(GlimsArea, ~ .x / 10^6 / Area)) |>
  ungroup()

# Emmeans -----------------------------------------------------------------
landcover_complete <-
  buchner_areas |>
  group_by(id) |>
  complete(Year = seq(1960, 2021, by = 1)) |>
  mutate(across(c(CroplandArea, ForestArea), ~ na_interpolation(.x))) |>
  ungroup()

all_df <-
  prec_year |>
  rename(id = river) |>
  left_join(
    sed_data |>
      rename(Year = year),
    by = join_by(id, Year)
  ) |>
  drop_na(ssd_mean) |>
  left_join(
    st_drop_geometry(ws_obs),
    by = join_by(id)
  ) |>
  left_join(
    landcover_complete,
    by = join_by(id, Year)
  ) |>
  left_join(
    glims_complete |>
      select(-Area),
    by = join_by(id, Year)
  ) |>
  mutate(
    ssy = ssd_mean * 31536 / area,
    across(c(CroplandArea, ForestArea), ~ .x / area)
  ) |>
  left_join(
    aal_long |>
      select(id, AAL, TotAban),
    by = join_by(id)
  )

good_id <-
  all_df |>
  group_by(id) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(ssd_mean ~ prec, data = .x)),
    coef = map_dbl(model, ~ broom::tidy(.x) |>
      filter(term == "prec") |>
      pull(estimate)),
    rsq = map_dbl(model, ~ broom::glance(.x)$r.squared)
  ) |>
  filter(rsq >= 0.3 & coef > 0) |>
  pull(id)

good_id

buchner_areas |>
  filter(id %in% good_id) |>
  left_join(
    st_drop_geometry(ws_obs),
    by = join_by(id)
  ) |>
  mutate(
    across(c(CroplandArea, ForestArea), ~ .x / area)
  ) |>
  pull(ForestArea) |>
  chop(c(0.5), labels = lbl_dash()) |>
  table()

glims_df |>
  mutate(GlimsArea = GlimsArea / 10^6 / Area) |>
  pull(GlimsArea) |>
  quantile()

good_df <-
  all_df |>
  filter(
    id %in% c(
      83314,
      83361,
      # 83350,
      83369,
      83310,
      83333,
      83307,
      83287,
      83229,
      83137,
      83210,
      83387
    )
  ) |>
  # filter(id %in% good_id) |>
  mutate(
    GlimsGroup = ifelse(GlimsArea < 0.01, "NG", "Glacier"),
    ForestGroup = santoku::chop(ForestArea, c(0.6), labels = lbl_dash()),
    CroplandGroup = santoku::chop(CroplandArea, c(0, 0.1, 0.2, 0.3), labels = lbl_dash()),
    AbanGroup = santoku::chop(
      AAL, c(0, 0.07, 1),
      left = TRUE,
      labels = lbl_dash()
    )
  )

hist(unique(good_df$TotAban))
table(good_df$AbanGroup)
table(good_df$ForestGroup)
table(good_df$CroplandGroup)
good_df |>
  distinct(id, CroplandArea) |>
  ggplot(aes(x = CroplandArea)) +
  geom_histogram()

m <-
  # lm(ssy ~ prec + prec*CroplandArea*ForestArea, data = good_df)
  lm(ssy ~ prec * CroplandArea * TotAban, data = good_df)
broom::glance(m)
broom::tidy(m)

plot_model(m,
  se = FALSE,
  type = "pred",
  terms = c("CroplandArea", "prec", "TotAban")
) +
  coord_cartesian(
    ylim = c(0, NA),
    # xlim = c(600, 1400),
    expand = FALSE
  )

emmeans(m, pairwise ~ CroplandArea | TotAban + prec,
  # cov.reduce = range,
  at = list(
    CroplandArea = c(0, 0.5, 1),
    prec = c(900, 1200, 1400),
    TotAban = c(0.2, 0.5, 0.7)
  )
)

em_ci <-
  emmeans(m, pairwise ~ CroplandArea | TotAban + prec,
    # cov.reduce = range,
    at = list(
      CroplandArea = seq(0, 0.8, 0.05),
      prec = c(900, 1200, 1400),
      TotAban = c(0.2, 0.5, 0.7)
    )
  )

emmeans_model_plot <-
  em_ci$emmeans |>
  as_tibble() |>
  mutate(
    prec = glue::glue("P = {prec} mm"),
    prec = as_factor(prec),
    TotAban = scales::percent(TotAban, accuracy = 1)
  ) |>
  ggplot(
    aes(
      x = CroplandArea,
      color = as_factor(TotAban),
      fill = as_factor(TotAban)
    )
  ) +
  geom_ribbon(
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    color = NA,
    alpha = 0.2
  ) +
  geom_line(
    aes(y = emmean),
    lwd = rel(1.1)
  ) +
  # coord_cartesian(
  #   ylim = c(0, NA),
  #   expand = TRUE
  # ) +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.07))
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 7),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(
    values = clrs[c(8, 4, 1)]
  ) +
  scale_fill_manual(
    values = clrs[c(8, 4, 1)]
  ) +
  facet_wrap(~prec) +
  labs(
    x = "**Cropland Area _CA_**",
    y = "**Suspended Sediment Yield<br>_SSY_, t/km<sup>2</sup>/yr**",
    color = "Abandoned Arable Land",
    fill = "Abandoned Arable Land"
  )

emmeans_model_plot

# Save
ggmw::mw_save(
  "figures/fig7_emmeans_plot.png",
  emmeans_model_plot,
  w = 24, h = 13
)


# Average Marginal Effects ------------------------------------------------
library(marginaleffects)

m_scaled <-
  good_df |>
  mutate(across(c(ssy, prec, CroplandArea, TotAban, ForestArea), ~ {
    datawizard::normalize(.x)
  })) %>%
  lm(ssy ~ prec * CroplandArea * ForestArea, data = .)
broom::glance(m_scaled)
broom::tidy(m_scaled)

plot_slopes(
  m_scaled,
  variables = "CroplandArea",
  condition = "prec"
)

avg_slopes(m_scaled, newdata = "mean")

plot_predictions(m_scaled, condition = list(
  "CroplandArea",
  "ForestArea" = "threenum",
  prec = "threenum"
))

plot_comparisons(m_scaled,
  variables = c("prec", "CroplandArea"),
  by = c("AbanGroup")
)

plot_predictions(m_scaled, by = c("AbanGroup"))

slopes(
  m,
  variables = "CroplandArea",
  newdata = datagrid(
    prec = c(800, 1000, 1400),
    TotAban = c(0.2, 0.5, 0.7)
  )
)

# -------------------------------------------------------------------------
good_df |>
  select(id, Year, prec, ssy, TotAban, CroplandArea) |>
  mutate(across(c(TotAban, CroplandArea), ~ {
    100 * .x
  })) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  gather(Type, Val, -id, -Year) |>
  ggplot(
    aes(
      x = as.factor(id)
    )
  ) +
  geom_boxplot(
    aes(y = Val)
  ) +
  facet_wrap(~Type, scales = "free")

all_df |>
  filter(
    id %in% c(
      83314,
      83361,
      # 83350,
      83369,
      83310,
      83333,
      83307,
      83287,
      83229,
      83137,
      83210,
      83387
    )
  ) |>
  filter(ssy < 900) |>
  ggplot(
    aes(
      x = prec,
      y = ssy
    )
  ) +
  geom_point(aes(color = id)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  ggpmisc::stat_poly_eq(formula = y ~ x + I(x^2))

ws_obs |>
  filter(
    id %in% c(
      83314,
      83361,
      # 83350,
      83369,
      83310,
      83333,
      83307,
      83287,
      83229,
      83137,
      83210,
      83387
    )
  ) |>
  mapview::mapview()
