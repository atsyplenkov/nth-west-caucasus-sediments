# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(rmapshaper)
library(exactextractr)
library(tictoc)
library(here)
library(lubridate)

# load data ------------------------------------------------------------
ws_sf <-
  sf::st_read(
    "data/vector/kbn_ws-predict/kbn_ws-predict.shp"
  )

ws_ext <-
  ws_sf |>
  ms_simplify() |>
  ms_dissolve() |>
  st_buffer(10000) |>
  st_transform(4326) |>
  vect() |>
  ext()

# Data paths --------------------------------------------------------------
# HYDE files
hyde_folders <-
  # list.dirs("C:/Users/atsyp/Downloads/HYDE/") %>%
  list.dirs("E:/WORK/00_GLOBAL/HYDE_3-2/baseline/Anthropocene") %>%
  keep(~ str_detect(.x, "(.*)(_lu)$")) %>%
  paste0(., "/")

# Forest change in 1987-2015
# Buchner et al., 2020
forest_file <-
  "E:/WORK/00_GLOBAL/Caucasus landuse/Caucasus_forest_change_map_1987-2015.tif"

# Cropland change in 1987-2015
# Buchner et al., 2020
cropland_file <-
  "E:/WORK/00_GLOBAL/Caucasus landuse/Caucasus_cropland_change_map_1987-2015.tif"

# Abandond areas
aal_path <-
  "E:/WORK/00_GLOBAL/hybrid_map_of_abandoned_arable_land_in_fSU/hybrid map of abandoned arable land.tif"

# WorldClim 2.1
wc_parent_path <-
  "E:/WORK/00_GLOBAL/WorldClim/histroical/5min/"

tmax_files <-
  fs::dir_ls(paste0(wc_parent_path, "/tmax"))
tmin_files <-
  fs::dir_ls(paste0(wc_parent_path, "/tmin"))
prec_files <-
  fs::dir_ls(paste0(wc_parent_path, "/prec"))

# WorldClim ---------------------------------------------------------------
tmax_rast <- rast(tmax_files)
tmin_rast <- rast(tmin_files)
prec_rast <- rast(prec_files)

tmax_mean <-
  exact_extract(
    tmax_rast,
    st_transform(ws_sf, 4326),
    "mean"
  )
tmin_mean <-
  exact_extract(
    tmin_rast,
    st_transform(ws_sf, 4326),
    "mean"
  )
prec_mean <-
  exact_extract(
    prec_rast,
    st_transform(ws_sf, 4326),
    "mean"
  )
prec_cv <-
  exact_extract(
    prec_rast,
    st_transform(ws_sf, 4326),
    "coefficient_of_variation"
  )
prec_med <-
  exact_extract(
    prec_rast,
    st_transform(ws_sf, 4326),
    "median"
  )

# Tmax
tmax_tidy <-
  tmax_mean |>
  mutate(river = ws_sf$river, .before = 1) |>
  gather(date, tmax, -river) |>
  as_tibble() |>
  mutate(
    date = str_remove_all(date, "mean.wc2.1_5m_tmax_|.tif"),
    date = lubridate::as_date(paste0(date, "-01"))
  )

tmax_year <-
  tmax_tidy |>
  group_by(river, Year = year(date)) |>
  reframe(
    tmax = mean(tmax)
  )

# Tmin
tmin_tidy <-
  tmin_mean |>
  mutate(river = ws_sf$river, .before = 1) |>
  gather(date, tmin, -river) |>
  as_tibble() |>
  mutate(
    date = str_remove_all(date, "mean.wc2.1_5m_tmin_|.tif"),
    date = lubridate::as_date(paste0(date, "-01"))
  )

tmin_year <-
  tmin_tidy |>
  group_by(river, Year = year(date)) |>
  reframe(
    tmin = mean(tmin)
  )

# Prec
prec_tidy <-
  prec_mean |>
  mutate(river = ws_sf$river, .before = 1) |>
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
    prec = mean(prec)
  )

# Prec CV
prec_cv_tidy <-
  prec_cv |>
  mutate(river = ws_sf$river, .before = 1) |>
  gather(date, prec_cv, -river) |>
  as_tibble() |>
  mutate(
    date = str_remove_all(date, "coefficient_of_variation.wc2.1_5m_prec_|.tif"),
    date = lubridate::as_date(paste0(date, "-01"))
  )

prec_cv_year <-
  prec_cv_tidy |>
  group_by(river, Year = year(date)) |>
  reframe(
    prec_cv = mean(prec_cv)
  )

# Prec Medain
prec_med_tidy <-
  prec_med |>
  mutate(river = ws_sf$river, .before = 1) |>
  gather(date, prec_med, -river) |>
  as_tibble() |>
  mutate(
    date = str_remove_all(date, "median.wc2.1_5m_prec_|.tif"),
    date = lubridate::as_date(paste0(date, "-01"))
  )

prec_med_year <-
  prec_med_tidy |>
  group_by(river, Year = year(date)) |>
  reframe(
    prec_med = median(prec_med)
  )

# WorldClim Annual dataset
wc_all <-
  list(
    prec_cv_year,
    prec_med_year,
    prec_year,
    tmax_year,
    tmin_year
  ) |>
  reduce(left_join, by = join_by(river, Year))

wc_all_month <-
  list(
    prec_cv_tidy,
    prec_med_tidy,
    prec_tidy,
    tmax_tidy,
    tmin_tidy
  ) |>
  reduce(left_join, by = join_by(river, date))

# Save
qs::qsave(
  list(wc_all, wc_all_month),
  "workflow/05_trend/data/worldclim.qs"
)

# Abandoned arable lands  -------------------------------------------------
aal_rast <-
  rast(aal_path)

aal_freqs <-
  exact_extract(
    aal_rast, st_transform(ws_sf, 4326),
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    }
  )

aal_tidy <-
  aal_freqs |>
  mutate(
    river = rep(ws_sf$river, each = 3),
    area = rep(as.numeric(st_area(ws_sf)) / 10^6, each = 3)
  ) |>
  mutate(type = case_when(
    value == 1 ~ "Cropland",
    value == 2 ~ "AAL",
    TRUE ~ "Other"
  )) |>
  mutate(area = freq * area)

aal_tidy |>
  filter(value != 3) |>
  select(river, type, freq, area) |>
  pivot_wider(
    id_cols = c(river),
    values_from = c(freq, area),
    names_from = type
  ) |>
  transmute(
    river,
    Cropland_2000 = area_Cropland + area_AAL,
    Cropland_2010 = area_Cropland
  )

# Forest ------------------------------------------------------------------
# Read forest raster
forest_rast <-
  rast(forest_file)

# Crop to AOI
# Mind the CRS!!!
forest_rast_crop <-
  crop(
    forest_rast,
    project(vect(ws_ext, crs = "EPSG:4326"), crs(forest_rast)),
    mask = T
  )

ws_david <-
  ws_sf |>
  st_transform(crs = crs(forest_rast_crop))

# Extract forest values
forest_freqs <-
  exact_extract(
    forest_rast_crop, ws_david,
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    }
  )

# Tidy up forest extraction
forest_freqs_tidy <-
  forest_freqs |>
  mutate(ID = ifelse(is.na(value), TRUE, FALSE)) |>
  mutate(IDr = data.table::rleid(ID)) |>
  mutate(IDr = ifelse(ID, lag(IDr), IDr)) |>
  mutate(IDr = data.table::rleid(IDr)) |>
  left_join(
    ws_david |>
      st_drop_geometry() |>
      select(river) |>
      rowid_to_column(var = "IDr") |>
      mutate(area = as.numeric(st_area(ws_sf)) / 10^6),
    by = join_by(IDr)
  ) |>
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
  select(river, area, type, freq) |>
  mutate(area = freq * area)

# Get forest areas for every year
forest_tidy <-
  forest_freqs_tidy |>
  filter(!type %in% c("no data", "no forest")) |>
  dplyr::select(river, area, type) |>
  group_by(river) %>%
  nest() %>%
  mutate(f1987 = map_dbl(
    data,
    ~ (.x$area[.x$type == "stable forest"] +
      .x$area[.x$type == "transitional forest"])
  )) %>%
  mutate(f1995 = map2_dbl(
    data, f1987,
    ~ (.y + .x$area[.x$type == "gain 1995"] -
      .x$area[.x$type == "loss 1995"])
  )) %>%
  mutate(f2000 = map2_dbl(
    data, f1995,
    ~ (.y + .x$area[.x$type == "gain 2000"] -
      .x$area[.x$type == "loss 2000"])
  )) %>%
  mutate(f2005 = map2_dbl(
    data, f2000,
    ~ (.y + .x$area[.x$type == "gain 2005"] -
      .x$area[.x$type == "loss 2005"])
  )) %>%
  mutate(f2010 = map2_dbl(
    data, f2005,
    ~ (.y + .x$area[.x$type == "gain 2010"] -
      .x$area[.x$type == "loss 2010"])
  )) %>%
  mutate(f2015 = map2_dbl(
    data, f2010,
    ~ (.y + .x$area[.x$type == "gain 2015"] -
      .x$area[.x$type == "loss 2015"])
  )) %>%
  dplyr::select(-data) %>%
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

# Crop to AOI
# Mind the CRS!!!
cropland_rast_crop <-
  crop(
    cropland_rast,
    project(vect(ws_ext, crs = "EPSG:4326"), crs(cropland_rast)),
    mask = T
  )

# Extract forest values
cropland_freqs <-
  exact_extract(
    cropland_rast_crop, ws_david,
    function(value, coverage_fraction) {
      data.frame(
        value = value,
        frac = coverage_fraction / sum(coverage_fraction)
      ) %>%
        group_by(value) |>
        summarize(freq = sum(frac))
    }
  )

# Tidy up forest extraction
cropland_freqs_tidy <-
  cropland_freqs |>
  mutate(ID = ifelse(is.na(value), TRUE, FALSE)) |>
  mutate(IDr = data.table::rleid(ID)) |>
  mutate(IDr = ifelse(ID, lag(IDr), IDr)) |>
  mutate(IDr = data.table::rleid(IDr)) |>
  left_join(
    ws_david |>
      st_drop_geometry() |>
      select(river) |>
      rowid_to_column(var = "IDr") |>
      mutate(area = as.numeric(st_area(ws_sf)) / 10^6),
    by = join_by(IDr)
  ) |>
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
  select(river, area, type, freq) |>
  mutate(area = freq * area)

# Get forest areas for every year
cropland_tidy <-
  cropland_freqs_tidy |>
  filter(!type %in% c("no data", "no cropland")) |>
  dplyr::select(river, area, type) |>
  group_by(river) %>%
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
    by = join_by(river, Year)
  )

# Save
qs::qsave(
  buchner_areas,
  "workflow/05_trend/data/landcover_buchner2020.qs"
)

# HYDE --------------------------------------------------------------------
get_hyde <-
  function(
      .path,
      .crs = 32637) {
    # Path to the files
    our_files <-
      list.files(.path,
        pattern = "(.*)(cropland|pasture|grazing|rangeland|conv_rangeland|ir_rice|ir_norice
                 tot_irri|tot_rice|tot_rainfed|rf_rice|rf_norice)(.*)(\\.asc)$",
        full.names = F
      )

    full_path <-
      paste0(.path, our_files)

    # Metadata of the HYDE files
    # YEAR
    year <-
      str_extract(our_files[1], "\\d{4,4}") %>%
      as.integer()

    # TYPES of the HYDE values
    type <-
      str_extract(
        our_files, "cropland|pasture|grazing|rangeland|conv_rangeland|ir_rice|ir_norice
                 tot_irri|tot_rice|tot_rainfed|rf_rice|rf_norice"
      )

    # Load HYDE rasters
    hyde_rast <-
      rast(full_path)

    # Crop HYDE rasters to the borders of watershed
    # buffer and project to the desired CRS
    hyde_crop <-
      hyde_rast %>%
      terra::crop(ws_ext) %>%
      terra::project(
        paste0("EPSG:", .crs)
      )

    # Estimate HYDE areas in km2
    hyde_areas <-
      exact_extract(
        hyde_crop,
        ws_sf,
        "sum"
      )

    names(hyde_areas) <- type
    hyde_areas$river <- ws_sf$river
    hyde_areas$year <- year

    hyde_areas
  }

# Run get_hyde
library(pbapply)

# approx. 2min
hyde_db <-
  pblapply(hyde_folders[3:32], get_hyde)

hyde_df <-
  collapse::unlist2d(hyde_db, idcols = FALSE)

# save
qs::qsave(
  hyde_df,
  "workflow/05_trend/data/hyde_2023.qs"
)

# GLIMS -------------------------------------------------------------------
library(geos)

ws_geos <-
  ws_sf |>
  as_geos_geometry()

glims_path <-
  "data/vector/glims.gpkg"

glims_sf <-
  st_read(glims_path) |>
  st_zm() |>
  transmute(glac_id, year = as.POSIXct(src_date, tz = "UTC") |> year()) |>
  st_transform(st_crs(ws_sf)) |>
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
      river = ws_sf$river,
      GlimsArea = glims_area
    )
  }

glims_df <-
  lapply(
    sort(unique(glims_tidy$year2)),
    get_glims_year
  ) |>
  bind_rows()

qs::qsave(
  glims_df,
  "workflow/05_trend/data/glims.qs"
)
