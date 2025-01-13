library(tidyverse)
library(data.table)
library(here)

source(here::here("R", "funs_ggplot2.R"))

ggplot2::theme_set(theme_kbn())

# Load data ----
list_df <- fs::dir_ls("data/tables/livestock", regexp = "*.csv$") |>
  as.list() |>
  lapply(fread)

list_names <- list.files("data/tables/livestock", pattern = "[.]csv$") |>
  stringr::str_remove("[.]csv$") |>
  stringr::str_split("_")

livestock_df <- map2(
  list_df,
  list_names,
  ~dplyr::mutate(
    .x,
    Region = .y[1],
    Type = .y[2],
    .after = 1
  )
) |>
  dplyr::bind_rows() |>
  dplyr::rename(Year = V1, Pop = V2)

# Rounding ----
livestock_imputed <- livestock_df |>
  dplyr::mutate(Year = round(Year)) |>
  dplyr::group_by(Region, Type) |>
  tidyr::complete(Year = seq(1990, 2019, by = 1)) |>
  dplyr::mutate(Pop = imputeTS::na_interpolation(Pop)) |>
  dplyr::ungroup()

# Explore population ----
livestock_imputed |>
  dplyr::filter(
    Year %in% c(1990, 2000, 2010)
  ) |>
  dplyr::group_by(Year, Type) |>
  dplyr::reframe(TotPop = sum(Pop) * 1000 / 10^6)
