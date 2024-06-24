library(tidyverse)
library(data.table)
library(here)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

# Load data ----
list_df <-
  fs::dir_ls("data/tables/livestock", regexp = "*.csv$") |>
  as.list() |>
  lapply(fread)

list_names <-
  list.files("data/tables/livestock", pattern = "[.]csv$") |>
  str_remove("[.]csv$") |>
  str_split("_")

livestock_df <-
  map2(
    list_df,
    list_names,
    ~ mutate(
      .x,
      Region = .y[1],
      Type = .y[2],
      .after = 1
    )
  ) |>
  bind_rows() |>
  rename(Year = V1, Pop = V2)

# Rounding ----
livestock_imputed <-
  livestock_df |>
  mutate(Year = round(Year)) |>
  group_by(Region, Type) |>
  complete(Year = seq(1990, 2019, by = 1)) |>
  mutate(Pop = imputeTS::na_interpolation(Pop)) |>
  ungroup()

# Explore population ----
livestock_imputed |>
  filter(
    Year %in% c(1990, 2000, 2010)
  ) |>
  group_by(Year, Type) |>
  reframe(TotPop = sum(Pop) * 1000 / 10^6)
