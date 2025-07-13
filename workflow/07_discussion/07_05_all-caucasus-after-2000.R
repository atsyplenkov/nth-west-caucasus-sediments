library(dplyr)
library(sf)
library(here)

# 1) Point data ---------------------------------------------------------
kbn_gages <- sf::st_read(
  here::here("data", "vector", "kbn_gages", "kbn_gages.shp"),
  query = "SELECT * FROM kbn_gages WHERE region = 'nw'"
) |>
  dplyr::mutate(id = as.character(id))

# Keep only "pristine" gages
rtop_ids <- kbn_gages |>
  dplyr::filter(is.na(status2)) |>
  dplyr::filter(id != "83361") |> # for validation
  dplyr::pull(id)

north_ids <- kbn_gages |>
  dplyr::pull(id)

# 2) Watersheds -----------------------------------------------------------
ws_ter <- sf::st_read(
  here::here("data/vector/ter_ws-6931/ter_ws-6931.shp"),
  quiet = TRUE
) |>
  dplyr::filter(is.na(status2)) |>
  dplyr::transmute(
    id = label,
    area
  )

ws_all <- sf::st_read(
  here::here("data/vector/kbn_ws_30dec/kbn_ws_30dec.shp"),
  query = "SELECT id, area FROM kbn_ws_30dec",
  quiet = TRUE
) |>
  dplyr::mutate(
    id = as.character(id)
  ) |>
  dplyr::filter(id %in% north_ids) |>
  sf::st_transform(6931) |>
  dplyr::bind_rows(ws_ter) |>
  rmapshaper::ms_simplify(keep = 0.3)

# Observed data
ws_obs <- ws_all |>
  dplyr::filter(id %in% rtop_ids | stringr::str_detect(id, "-"))

# 3) Sediment data --------------------------------------------------------
ter_data <- readxl::read_excel(
  here::here("data/hydro/terek_SY.xlsx"),
  sheet = "data_kgs",
  range = "A2:AW96"
) |>
  dplyr::rename(year = 1) |>
  tidyr::gather(id, ssd_mean, -year) |>
  dplyr::relocate(year, .after = id)

sed_data <- qs::qread(
  here::here(
    "workflow",
    "01_sediment-database",
    "data",
    "SSD-yr-all_21dec23.qs"
  )
) |>
  dplyr::mutate(id = as.character(id)) |>
  dplyr::select(id:ssd_mean) |>
  dplyr::bind_rows(
    ter_data
  )

# 4) Merge data -----------------------------------------------------------
ws_obs_sy <- ws_obs |>
  dplyr::left_join(
    sed_data,
    by = dplyr::join_by(id)
  ) |>
  dplyr::mutate(ssy = ssd_mean * 31536 / area) |>
  tidyr::drop_na(ssy)

ws_obs_2000 <-
  ws_obs_sy |>
  dplyr::filter(year >= 2000)

ws_obs_2000_sf <-
  ws_obs_2000 |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
  dplyr::mutate(geometry = wk::as_wkt(ws_obs_2000)) |>
  dplyr::group_by(id, geometry) |>
  dplyr::reframe(
    mean_ssy = mean(ssy, na.rm = TRUE), # in t/km2/yr
    start = min(year),
    end = max(year),
    n = dplyr::n()
  ) |>
  sf::st_as_sf(wkt = "geometry", crs = sf::st_crs(ws_obs_sy))

# Plot -------------------------------------------------------------------
mapview::mapview(
  ws_obs_2000_sf,
  zcol = "mean_ssy",
  layer.name = "SSY, t/km²/yr"
)

# Save -------------------------------------------------------------------
st_write(
  obj = ws_obs_2000_sf,
  dsn = "data/caucasus_ssy_after_2000.gpkg",
  layer = "catchments"
)
