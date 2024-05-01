library(tidyverse)
library(readxl)
library(sf)
library(mapview)

# 1) Load AIS GMVO data ---------------------------------------------------
krasn_abrasion <- 
  read_excel(
    "data/tables/report_2024-01-21.xlsx",
    sheet = "Sheet2"
  ) |> 
  select(-Coordinates) |> 
  drop_na(Year) |> 
  mutate(dX = str_replace(dX, ",", "."),
         dX = as.numeric(dX))

# 2) Transform to spatial data --------------------------------------------
kras_abrasion_sf <- 
  st_as_sf(
    krasn_abrasion,
    coords = c("X", "Y"),
    crs = 4326
  )

mapview(kras_abrasion_sf)

st_write(
  kras_abrasion_sf,
  "data/vector/krasnodar_abrasion/krasnodar_abrasion.shp",
  layer_options = "ENCODING=UTF-8",
  delete_layer = TRUE
)
