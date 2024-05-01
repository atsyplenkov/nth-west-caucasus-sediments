library(tidyverse)
library(qs)

source("R/funs_utils.R")

# 1) Load data ------------------------------------------------------------
rtop_data <- 
  qs::qread(
    "workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs"
  )

bedload_model <- 
  qs::qread(
    "workflow/02_rtop-interpolation/data/bedload_model.qs"
  )

# I was too lazy to properly call this nls model in the 
# future pipeline. 
# So this is just to be sure that later on lines 30-32 
# the correct bedload model is called
summary(bedload_model)

# 2) Tidy data ------------------------------------------------------------
rtop_pred <- 
  rtop_data |> 
  select(.pred_df) |> 
  unnest(cols = c(.pred_df)) |> 
  transmute(
    year,
    id,
    area,
    ssd_sim = var1.pred^(1/0.05), # Mind the Box-Cox lambda
    ssd_sim = ssd_sim * area / 31536,
    bed_f = 28.21 / (95.25 + ssd_sim),
    tot_sd = ssd_sim / (1 - bed_f),
    ssd_tyr = ssd_sim * 31536 / 10^6,
    tot_tyr = tot_sd * 31536 / 10^6
  )
# 3) Inflow Summary -------------------------------------------------------
# Subset only those gauging stations that have 
# inflows directly into the Krasnodar reservoir.
krasn_inflow <- 
  rtop_pred |> 
  filter(
    id %in% c(
      "83174",
      "83314",
      "83361",
      "83387",
      "Marta",
      "Apchas",
      "Shunduk",
      "Psekups"
    )
  )

krasn_inflow_summary <- 
  krasn_inflow |> 
  mutate(
    year_period = case_when(
      between(year, 2005, 2016) ~ "2005-2016",
      between(year, 2017, 2021) ~ "2016-2021",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(year_period)) |> 
  summarise(
    area = unique(area),
    mean_ssd = mean(ssd_sim),
    mean_bf = mean(bed_f),
    tot_ssd_tyr = sum(ssd_tyr),
    tot_sd_tyr = sum(tot_tyr),
    .by = c(year_period, id)
  )

krasn_inflow_summary |> 
  # filter(id != "83361") |>
  mutate(
    tot_sd_tyr = case_when(
      id == "83361" ~ tot_sd_tyr / 2,
      TRUE ~ tot_sd_tyr)
  ) |> 
  mutate(
    tot_ssd_tyr  = case_when(
      id == "83361" ~ tot_ssd_tyr  / 2,
      TRUE ~ tot_ssd_tyr )
  ) |> 
group_by(year_period) |> 
  summarise(
    ss_flux = sum(tot_ssd_tyr),
    bs_flux = sum(tot_sd_tyr) - sum(tot_ssd_tyr),
    s_flux = sum(tot_sd_tyr)
  ) |> 
  mutate(
    ss_vol = ss_flux / 0.96,
    bs_vol = bs_flux / 1.7,
    # s_vol = s_flux / 1.3
    s_vol = ss_vol + bs_vol
  )
  # mutate(
  #   ss_vol = sed_to_vol(ss_flux),
  #   s_vol = sed_to_vol(s_flux)
  # )

krasn_inflow_summary |> 
  filter(id == "83361") |>
  group_by(year_period) |> 
  summarise(
    ss_flux = sum(tot_ssd_tyr/2),
    s_flux = sum(tot_sd_tyr/2)
  ) |> 
  mutate(
    ss_vol = sed_to_vol(ss_flux),
    s_vol = sed_to_vol(s_flux)
  )

# Kuban only
krasn_inflow_summary |> 
  filter(id %in% c("83174", "83314")) |>
  group_by(year_period) |> 
  summarise(
    ss_flux = sum(tot_ssd_tyr),
    s_flux = sum(tot_sd_tyr)
  ) |> 
  mutate(
    ss_vol = sed_to_vol(ss_flux),
    s_vol = sed_to_vol(s_flux)
  )

rtop_pred |> 
  filter(id == "83183") |>  
  # filter(year > 2000) |>  
  mutate(
    ssy = ssd_sim * 31536 / area
  ) |> 
  ggplot(
    aes(
      year,
      ssy
    )
  ) +
  geom_line() +
  geom_smooth(method = "lm")
