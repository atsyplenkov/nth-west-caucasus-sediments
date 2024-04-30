library(here)
library(tidyverse)
library(ggnewscale)
library(latex2exp)
library(trend)
library(ChangePointTaylor)
library(geomtextpath)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())
# theme_replace(
#   axis.title.y = element_text(size = rel(0.9), 
#                               angle = 90)
# )

clrs <- MetBrewer::met.brewer("Johnson", n = 8)
clrs[3] <- "grey10"

# 1) Load data ------------------------------------------------------------
rtop_data <- 
  qs::qread(
    "workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs"
  )

bedload_model <- 
  qs::qread(
    "workflow/02_rtop-interpolation/data/bedload_model.qs"
  )

sed_data <- 
  qs::qread(
    here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs")
  ) |> 
  transmute(
    id = as.character(id),
    year,
    ssd_mean
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

# Compare measured and imputed data
krasn_inflow |> 
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |> 
  ggplot(
    aes(
      x = ssd_mean,
      y = ssd_sim
    )
  ) +
  geom_point() +
  geom_abline() +
  tune::coord_obs_pred()


krasn_both <- 
  krasn_inflow |> 
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |> 
  mutate(
    SSDBoth = ifelse(is.na(ssd_mean), ssd_sim, ssd_mean),
    SSDFlag = ifelse(is.na(ssd_mean), "Sim", "Obs")
  ) |> 
  mutate(
    bed_f = 28.21 / (95.25 + SSDBoth),
    tot_sd = SSDBoth / (1 - bed_f),
    ssd_tyr = SSDBoth * 31536 / 10^6,
    tot_tyr = tot_sd * 31536 / 10^6
  ) |> 
  mutate(
    id = case_when(
      id == "83174" ~ glue::glue("Kuban (No. {id})"),
      id == "83314" ~ glue::glue("Laba (No. {id})"),
      id == "83361" ~ glue::glue("Belaya (No. {id})"),
      id == "83387" ~ glue::glue("Pshish (No. {id})"),
      TRUE ~ id
    )
  )

# Save
krasn_both |> 
  transmute(
    Year = year,
    id,
    river = str_split_i(id, " \\(", i = 1),
    Area = area,
    SSDBoth,
    SSDFlag,
    BedloadF = bed_f,
    SDTotal = SSDBoth / (1 - bed_f)
  ) |> 
  qs::qsave(
    "workflow/05_trend/data/ssd_both.qs"
  )

krasn_inflow_summary <- 
  krasn_both |> 
  mutate(
    Period = case_when(
      between(year, 2004, 2015) ~ "2005-2016",
      between(year, 2016, 2021) ~ "2016-2021",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(Period)) |> 
  summarise(
    A = unique(area),
    SSDmean = mean(SSDBoth),
    BFmean = mean(bed_f),
    SSDtot = sum(ssd_tyr),
    SDtot = sum(tot_tyr),
    .by = c(Period, id)
  )

krasn_budget <- 
  krasn_inflow_summary |> 
  mutate(
    SDtot = case_when(
      id == "83361" ~ SDtot * 1,
      TRUE ~ SDtot)
  ) |> 
  mutate(
    SSDtot  = case_when(
      id == "83361" ~ SSDtot * 1,
      TRUE ~ SSDtot )
  ) |> 
  group_by(Period) |> 
  summarise(
    FluxSS = sum(SSDtot),
    FluxBS = sum(SDtot) - sum(SSDtot),
    FluxTot = sum(SDtot)
  ) |> 
  mutate(
    V = case_when(
      Period == "2005-2016" ~ 83,
      TRUE ~ 24.66
    ),
    Shore = case_when(
      Period == "2005-2016" ~ 0.79,
      TRUE ~ 0.22
    ),
    .after =  1
  ) |> 
  mutate(
    Income = FluxTot + Shore * 1.7,
    Storage = V * 0.96,
    Error = 100 * (Income - Storage) / Storage
  )

krasn_budget

krasn_both |>
  group_by(id) |> 
  complete(year = seq(1925, 2021, by = 1))

# Pettitt test ------------------------------------------------------------
pett <-
  krasn_both |>
  group_by(id) |>  
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = T) |>
  mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |> 
  mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |> 
  nest() |>  
  mutate(pt = map(data,
                      ~trend::pettitt.test(.x$SSDBoth)$estimate)) |> 
  mutate(p = map(data,
                     ~trend::pettitt.test(.x$SSDBoth)$p.value)) |>  
  unnest(cols = c(pt, p)) |> 
  mutate(start = map_dbl(data, ~first(.x$year)),
         end = map_dbl(data, ~last(.x$year))) |> 
  select(-data) |>  
  ungroup() |>  
  mutate(p_factor = ifelse(p < 0.05, "sign", "non")) |> 
  rowwise() |> 
  mutate(break_year = seq(start, end)[pt]) |> 
  ungroup()

set.seed(1234)
taylor_res <- 
  krasn_both |>
  group_by(id) |>
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = T) |> 
  mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |> 
  mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |> 
  nest() |> 
  mutate(taylor = map(data,
                      ~change_point_analyzer(
                        x = .x$SSDBoth,
                        labels = .x$year,
                        min_candidate_conf = 0.98,
                        min_tbl_conf = 0.95,
                        n_bootstraps = 5000,
                        CI = 0.95
                        # method = "CUSUM"
                      ))) |> 
  dplyr::select(-data) |>  
  unnest(cols = c(taylor)) |> 
  ungroup()

taylor_ci <- 
  taylor_res |> 
  mutate(
    label_low = str_split(`CI (95%)`, "-", simplify = T)[,1],
    label_upper = str_split(`CI (95%)`, "-", simplify = T)[,2],
  ) |> 
  mutate(across(contains("label_"), ~parse_number(.x)))

print(taylor_ci, n = 100)

# CUSUM -------------------------------------------------------------------
krasn_cusum <- 
  krasn_both |>
  group_by(id) |>
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = T) |>
  mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |>
  mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |>
  mutate(
    SSDFlag = ifelse(year == "2021", "Sim", SSDFlag)
  ) |> 
  ungroup() |> 
  select(year, id, SSDBoth, SSDFlag) |> 
  group_by(id)  |> 
  mutate(SSDMean = mean(SSDBoth, na.rm = T),
         SSDCv = sd(SSDBoth, na.rm = T)/SSDMean) |>  
  mutate(K = SSDBoth/SSDMean - 1) |> 
  mutate(CDC = cumsum(K)/SSDCv) |>  
  ungroup()

krasn_cusum_plot <- 
  krasn_cusum |> 
  filter(!is.na(SSDFlag)) |> 
  ggplot(
    aes(
      x = year,
      y = CDC
    )
  ) +
  geomtextpath::geom_textvline(
    data = pett,
    aes(
      label = break_year,
      xintercept = break_year,
      color = "Pettitt"
    ),
    vjust = -0.1,
    hjust = 0.05,
    size = 2.5,
    family = "Inter",
    fontface = "bold",
    key_glyph = "timeseries"
  ) +
  geom_rect(
    data = taylor_ci,
    aes(
      xmin = label_low,
      xmax = label_upper,
      ymin = -Inf,
      ymax = Inf,
      fill = "95% CI"
    ),
    alpha = 0.2,
    inherit.aes = F,
    key_glyph = draw_key_rect
  ) +
  geomtextpath::geom_textvline(
    data = taylor_ci,
    aes(
      label = label,
      xintercept = label,
      color = "Taylor"
    ),
    vjust = 1.05,
    hjust = 0.3,
    size = 2.5,
    family = "Inter",
    fontface = "bold",
    key_glyph = "timeseries"
  ) +
  scale_color_manual(
    name = "Change point detection method",
    values = clrs[c(3, 1)],
    labels = c("Pettitt, 1979", "Taylor, 2000")
  ) +
  scale_fill_manual(
    name = "Change point detection method",
    values = clrs[c(1)],
    labels = c("95% CI (Taylor, 2000)")
  ) +
  guides(
    fill = guide_legend(
      title = "",
      order = 2,
      ncol = 1,
      title.position = "top"
    ),
    color = guide_legend(
      order = 1,
      ncol = 1,
      title.position = "top"
    )
  ) +
  # scale_fill_manual(
  #   values = clrs[c(1, 4, 6)],
  #   labels = c("95% CI (Taylor, 2000)", "Observed", "Predicted")
  # ) +
  ggnewscale::new_scale_fill() +
  geom_line() +
  geom_point(
    aes(fill = SSDFlag),
    shape = 21,
    stroke = rel(0.25)
  ) +
  scale_fill_manual(
    name = "Suspended sediment load",
    values = clrs[c(4, 6)],
    labels = c("Observed", "Predicted")
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    limits = c(1925, 2021),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  facet_wrap(
    ~ id,
    scales = "free_y"
  ) +
  labs(
    x = "",
    # y = TeX(r'($\frac{\sum{K-1}}{Cv}$)')
    y = "**CUSUM _SSD_**"
  ) +
  guides(
    fill = guide_legend(
      order = 3,
      ncol = 1,
      title.position = "top"
    ))

krasn_cusum_plot

ggmw::mw_save(
  "figures/fig5_krasnodar-inflow-cusum.png",
  krasn_cusum_plot,
  w = 22, h = 13
)

# Trends ------------------------------------------------------------------
taylor_periods <- 
  taylor_res |> 
  select(id, label) |> 
  bind_rows(
    data.frame(
      id = rep(unique(taylor_res$id), 2),
      label = c(rep(1925, 8), rep(2021, 8))
    )
  ) |> 
  arrange(id, label) |> 
  group_by(id) |> 
  mutate(
    Period = data.table::rleid(label),
    Period = ifelse(label == 2021, NA_integer_, Period)
  ) |> 
  complete(label = seq(1925, 2021, by = 1)) |> 
  mutate(Period = zoo::na.locf(Period)) |> 
  ungroup()

krasn_period <- 
  krasn_both |> 
  left_join(
    taylor_periods |> 
      select(id, year = label, Period),
    by = join_by(id, year)
  )

krasn_period |> 
  ggplot(
    aes(
      x = year,
      y = SSDBoth,
      fill = as_factor(Period)
    )
  ) +
  geom_point(
    shape = 21,
    stroke = rel(0.25)
  ) +
  geom_smooth(
    method = "lm",
    se = F
  ) +
  scale_y_log10()+
  facet_wrap(~id, scales = "free_y")
