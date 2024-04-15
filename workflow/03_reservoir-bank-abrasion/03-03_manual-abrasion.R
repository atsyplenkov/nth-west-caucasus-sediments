library(terra)

# Paths -------------------------------------------------------------------
poly2006 <- 
  "workflow/03_reservoir-bank-abrasion/data/vector/polygons_2006_2016.shp"
poly2016 <- 
  "workflow/03_reservoir-bank-abrasion/data/vector/polygons_2016_2021.shp"

# Load layers -------------------------------------------------------------
v2006 <- 
  vect(poly2006)

v2016 <- 
  vect(poly2016)

# Area --------------------------------------------------------------------
v2006_area <- 
  expanse(v2006, "m") |> 
  sum()

v2016_area <- 
  expanse(v2016, "m") |> 
  sum()

# Volume ------------------------------------------------------------------

tot_vol_2006 <- v2006_area * 5
tot_vol_2006 / 10^6

tot_vol_2016 <- v2016_area * 5
tot_vol_2016 / 10^6

# Mean annual rate -------------------------------------------------------
diff_years_2016 <- 
  difftime(
    as.Date("2021-07-31"),
    as.Date("2016-10-01")
  ) / 365

tot_vol_a_2016 <- 
  tot_vol_2016 / as.numeric(diff_years_2016)
tot_vol_a_2016 * 6 / 10^6

diff_years_2006 <- 
  difftime(
    as.Date("2016-10-01"),
    as.Date("2006-08-06")
  ) / 365

tot_vol_a_2006 <- 
  tot_vol_2006 / as.numeric(diff_years_2006)
tot_vol_a_2006 * 12 / 10^6
