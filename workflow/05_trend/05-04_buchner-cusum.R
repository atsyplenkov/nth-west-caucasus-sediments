library(tidyverse)
library(here)
library(lubridate)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

# Load data ---------------------------------------------------------------
buchner_areas <- 
  qs::qread("workflow/05_trend/data/landcover_buchner2020.qs")

hyde_df <-
  qs::qread("workflow/05_trend/data/hyde_2023.qs")

glims_df <-
  qs::qread("workflow/05_trend/data/glims.qs")

wc_all <- 
  qs::qread("workflow/05_trend/data/worldclim.qs") |> 
  pluck(2) |>
  select(river, date, prec) |> 
  group_by(river, Year = year(date)) |> 
  reframe(
    Precipitation = sum(prec)
  )

# Extend the Cropland Area ------------------------------------------------
hyde_lm <- 
  buchner_areas |> 
  left_join(
    hyde_df |> 
      select(river, Year = year, cropland),
    by = join_by(river, Year)
  ) %>% 
  lm(CroplandArea ~ cropland, data = .)

buchner_hyde <- 
  hyde_df |> 
  select(river, Year = year, cropland) |> 
  group_by(river) |> 
  complete(Year = seq(1920, 2021, by = 1)) |> 
  mutate(cropland = na_interpolation(cropland)) |>
  ungroup() |> 
  left_join(
    buchner_areas,
    by = join_by(river, Year)
  ) |> 
  augment(x = hyde_lm, data = .) |> 
  filter(
    !is.na(CroplandArea) |
      Year %in% c(1960, 1965, 1970, 1975, 1980)
  ) |> 
  mutate(
    Crops = ifelse(is.na(CroplandArea), .fitted, CroplandArea)
  )

# Quick CUSUM -------------------------------------------------------------
hyde_cusum <- 
  hyde_df |> 
  select(river, Year = year, cropland) |>
  group_by(river) |> 
  complete(Year = seq(1920, 2021, by = 5)) |> 
  mutate(cropland = na_interpolation(cropland)) |> 
  ungroup() |> 
  filter(Year >= 1987) |> 
  gather(Type, Area, -river, -Year) |> 
  group_by(river, Type) |> 
  mutate(CDC = cumsum((Area-mean(Area))/sd(Area))) |> 
  ungroup()

buchner_cusum <- 
  buchner_areas |> 
  group_by(river) |> 
  complete(Year = seq(1987, 2015, by = 1)) |> 
  mutate(
    across(c(CroplandArea, ForestArea),
           ~na_interpolation(.x))
  ) |> 
  ungroup() |> 
  gather(Type, Area, -river, -Year) |> 
  group_by(river, Type) |> 
  mutate(CDC = cumsum((Area-mean(Area))/sd(Area))) |> 
  ungroup()

worldclim_cusum <- 
  wc_all |> 
  # filter(between(Year, 1987, 2015)) |> 
  gather(Type, Var, -river, -Year) |> 
  group_by(river, Type) |> 
  mutate(CDC = cumsum((Var-mean(Var))/sd(Var))) |> 
  ungroup()

glims_cusum <- glims_df |> 
  filter(GlimsArea != 0) |> 
  group_by(river) |> 
  complete(Year = seq(1960, 2020, by = 1)) |> 
  mutate(GlimsArea = na_interpolation(GlimsArea)/10^6) |> 
  # filter(between(Year, 1987, 2015)) |> 
  gather(Type, Var, -river, -Year) |> 
  group_by(river, Type) |> 
  # mutate(VarMean = mean(Var, na.rm = T),
  #        VarCv = sd(Var, na.rm = T) / VarMean) |>  
  # mutate(K = Var / VarMean - 1) |> 
  # mutate(CDC = cumsum(K) / VarCv) |>
  mutate(CDC = cumsum((Var-mean(Var))/sd(Var))) |> 
  ungroup()

buchner_hyde_cusum <- 
  buchner_hyde |> 
  select(river, Year, CroplandArea) |> 
  gather(Type, Var, -river, -Year) |> 
  group_by(river, Type) |> 
  mutate(CDC = cumsum((Var-mean(Var))/sd(Var))) |> 
  ungroup()

buchner_cusum |> 
  filter(Type != "ForestArea") |> 
  bind_rows(worldclim_cusum) |>
  # bind_rows(glims_cusum) |>
  # bind_rows(hyde_cusum) |> 
  # filter(Year >= 1987) |> 
  ggplot(
    aes(
      x = Year,
      y = CDC,
      fill = river,
      color = river,
      group = river
    )
  ) +
  geom_line()
  # geom_point(
  #   shape = 21,
  #   color = "grey20",
  #   stroke = rel(0.25)
  # ) +
  facet_wrap(~river, scales = "free_y")

# GLIMS -------------------------------------------------------------------
glims_cusum <- glims_df |> 
  filter(GlimsArea != 0) |> 
  gather(Type, Var, -river, -Year) |> 
  group_by(river, Type) |> 
  mutate(VarMean = mean(Var, na.rm = T),
         VarSD = sd(Var, na.rm = T)) |>
  # mutate(z = (Var - VarMean)/VarSD,
         # CDC = cumsum(z)) |>
  # mutate(K = (Var / VarMean) - 1) |> 
  # mutate(CDC = cumsum(K) / VarCv) |>  
  ungroup()

glims_cusum |> 
  ggplot(
    aes(
      x = Year,
      y = CDC
    )
  ) +
  geom_line() +
  facet_wrap(~river, scales = "free_y")

buchner_areas |> 
  left_join(
    hyde_df |> 
      select(river, Year = year, pasture, cropland, grazing),
    by = join_by(river, Year)
  ) |>
  mutate(CroplandHYDE = cropland) |> 
  ggplot(
    aes(
      x = CroplandHYDE,
      y = CroplandArea
    )
  ) +
  geom_point() +
  geom_abline() +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq(
    use_label(c("eq", "R2"))
  ) +
  scale_x_log10() +
  scale_y_log10()

# Prec Trends -------------------------------------------------------------
wc_all |> 
  filter(river == "Pshish") |> 
  mutate(Period = santoku::chop(Year, 1986, santoku::lbl_dash())) |> 
  ggplot(
    aes(x = Year,
        y = Precipitation,
        color = Period)
  ) +
  geom_point() +
  geom_smooth(
    aes(x = Year,
        y = Precipitation),
    method = "lm", 
    inherit.aes = F)

library(ChangePointTaylor)
set.seed(1234)
# taylor_res <- 
  wc_all |>
    filter(river == "Pshish") |> 
  group_by(river) |>
  nest() |> 
  mutate(taylor = map(data,
                      ~change_point_analyzer(
                        x = .x$Precipitation,
                        labels = .x$Year,
                        min_candidate_conf = 0.3,
                        min_tbl_conf = 0.5,
                        n_bootstraps = 1000
                        # CI = 0.95
                        # method = "CUSUM"
                      ))) |> 
  dplyr::select(-data) |>  
  unnest(cols = c(taylor)) |> 
  ungroup()

