# libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(atslib)
library(here)
library(qs)
library(mapview)
library(sf)
library(rmapshaper)
library(mapview)

source(here("R", "funs_ggplot2.R"))
source(here("R", "funs_excel.R"))

theme_set(theme_kbn())

# 0. Annual SSD timeseries ------------------------------------------------
ssd_raw <- 
  qread(here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs"))

gage_id <- 
  read.xlsx(here("data", "hydro", "gage_id-24082022.xlsx")) %>% 
  as_tibble()

# 1. Gage locations -------------------------------------------------------
ws_st <-
  st_read(
    here("data", "vector", "kbn_ws_30dec", "kbn_ws_30dec.shp")
  ) %>% 
  mutate(
    area = st_area(.),
    area = as.numeric(area) / 10^6 # km2
  ) 

aoi <- 
  ws_st %>% 
  rmapshaper::ms_simplify() %>% 
  st_union() %>% 
  st_buffer(5000) %>%
  st_transform(4326) %>% 
  st_bbox()

# 2. Area check -----------------------------------------------------------
# ws_st %>%
#   rename(area_dem = area) %>%
#   right_join(gage_id, by = "id") %>%
#   drop_na(area_dem) %>%
#   ggplot(aes(area, area_dem)) +
#   geom_point() 

# 3. Reservoir check ------------------------------------------------------
ww_reservoirs <- 
  st_read("D:/WORK/00_GLOBAL/WaterWatch/shp/reservoirs-locations-v1.0.shp")

manual_reservoirs <- 
  c(83265, 83266, 83269, 83444)

ww_reservoirs_bb <- 
  ww_reservoirs %>% 
  st_crop(aoi)

ww_utm <- 
  ww_reservoirs_bb %>% 
  st_transform(32637) %>% 
  dplyr::select(fid)

pristine_id <- 
  sf::st_join(ws_st, ww_utm) %>% 
  filter(is.na(fid)) %>% 
  filter(!id %in% manual_reservoirs) %>% 
  pull(id)
  
pristine_id_data <- 
  ssd_raw %>% 
  filter(id %in% pristine_id) %>% 
  pull(id) %>% 
  unique() 

# 4. Station selection ----------------------------------------------------
ancient_only_id_data <- 
  c(83307, 83333, 83413, 83387, 83436, 83287)

ssd_id <- 
  ssd_raw %>% 
  filter(id %in% pristine_id_data) %>% 
  filter(!id %in% ancient_only_id_data) %>% 
  group_by(id) %>% 
  mutate(n = sum(!is.na(ssd_mean))) %>% 
  ungroup() %>% 
  filter(n > 20) %>% 
  pull(id) %>% 
  unique()


# 5. Explore --------------------------------------------------------------
ws_st %>% 
  filter(id %in% ssd_id) %>% 
  mapview(alpha.region = 0)

ssd_raw %>%   
  filter(id %in% ssd_id) %>% 
  explore_miss(ssd_mean)

# Database
ssd_selected <- 
  ssd_raw %>%   
  dplyr::filter(id %in% ssd_id)

ssd_table <- 
  ssd_selected %>% 
  drop_na(ssd_mean) %>% 
  group_by(id) %>% 
  nest() %>% 
  mutate(mean_available = map_chr(data,
                                 ~years_range(.x))) %>% 
  unnest(cols = c(data)) %>% 
  summarise(
    n_year = sum(!is.na(ssd_mean)),
    range_year = first(mean_available),
    ssd_av = .mean_na(ssd_mean),
    ssd_mad = .mad_na(ssd_mean),
    ssd_max = .max_na(ssd_max),
    .groups = "drop"
  ) %>% 
  left_join(
    gage_id,
    by = "id"
  ) %>% 
  dplyr::select(
    id, river, gage, area,
    everything(),
    -aid, -comment, -`ОГХ`
  ) %>% 
  arrange(river, area)

# 6. Save -----------------------------------------------------------------
# Table
writexl::write_xlsx(
  ssd_table,
  here("R", "01_Sediment-database", "tables", "SSD-table-1.xlsx")
)

# Database
qsave(ssd_selected,
      here("R", "01_Sediment-database", "data", "SSD-yr-sel_1mar23.qs"))

# Watersheds
ws_st %>% 
  dplyr::filter(id %in% ssd_id) %>% 
  st_write(
    dsn = here("R", "01_Sediment-database", "data", "SSD-ws-sel_1mar23.shp")
  )
