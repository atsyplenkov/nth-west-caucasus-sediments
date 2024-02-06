library(tidyverse)
library(sf)

# 1) Load polygon data ----------------------------------------------------
# 1984-2021 change
gswe_poly <- 
  st_read(
    "data/vector/gswe.gpkg",
    layer = "gswe"
  ) |> 
  st_transform(32637)

# estimate polygon area
gswe_poly$area <- 
  st_area(gswe_poly)

# 2) Estimate volume ------------------------------------------------------
# total sum of eroded area
gswe_tot_area <- 
  sum(gswe_poly$area)

# multiply by mean bank height
gswe_tot_vol <- gswe_tot_area*5.9

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
