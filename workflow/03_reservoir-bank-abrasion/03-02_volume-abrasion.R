library(tidyverse)
library(sf)

# 1) Load polygon data ----------------------------------------------------
# 1984-2021 change
gswe_poly <-
  st_read(
    "data/vector/gswe.gpkg",
    layer = "gswe"
  ) |>
  st_transform(32637) |>
  filter(part == "north")

# estimate polygon area
gswe_poly$area <-
  st_area(gswe_poly)

# 2) Estimate volume ------------------------------------------------------
# total sum of eroded area
gswe_tot_area <-
  sum(gswe_poly$area)

# EDA of bank heights
tmp_kml <- fs::path_temp()
unzip(
  "data/vector/krasnodar_banks/krasnodar_banks.kmz",
  exdir = tmp_kml
)
bank_heights <-
  sf::st_read(
    dsn = fs::path_join(c(tmp_kml, "doc.kml")),
    quiet = TRUE
  ) |>
  sf::st_zm()

summary(
  sort(
    as.double(bank_heights$Name)
  )[-8:-9]
)

# multiply by mean bank height
gswe_tot_vol <- gswe_tot_area * 5.9

# convert to 2004-2016 erosion rates
tot_vol <-
  gswe_tot_vol * 0.51

# 3) Mean annual rate -----------------------------------------------------
diff_years <-
  difftime(
    as.Date("2016-08-01"),
    as.Date("2021-08-01")
  ) / 365

tot_vol_a <-
  tot_vol / 12
