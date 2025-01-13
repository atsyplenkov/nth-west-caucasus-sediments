library(here)
library(tidyverse)
library(ggnewscale)
library(trend)
library(ChangePointTaylor)
library(geomtextpath)

source(here::here("R", "funs_ggplot2.R"))

ggplot2::theme_set(theme_kbn())

# theme_replace(
#   axis.title.y = element_text(size = rel(0.9),
#                               angle = 90)
# )

clrs <- MetBrewer::met.brewer("Johnson", n = 8)
clrs[3] <- "grey10"

# 1) Load data ------------------------------------------------------------
rtop_data <- qs::qread(
  "workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs"
)

bedload_model <- qs::qread(
  "workflow/02_rtop-interpolation/data/bedload_model.qs"
)

sed_data <- qs::qread(
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
rtop_pred <- rtop_data |>
  dplyr::select(.pred_df) |>
  tidyr::unnest(cols = c(.pred_df)) |>
  dplyr::transmute(
    year,
    id,
    area,
    ssd_sim = var1.pred^(1 / 0.05), # Mind the Box-Cox lambda
    ssd_sim = ssd_sim * area / 31536,
    bed_f = 28.21 / (95.25 + ssd_sim),
    tot_sd = ssd_sim / (1 - bed_f),
    ssd_tyr = ssd_sim * 31536 / 10^6,
    tot_tyr = tot_sd * 31536 / 10^6
  )

# 3) Inflow Summary -------------------------------------------------------
# Subset only those gauging stations that have
# inflows directly into the Krasnodar reservoir.
krasn_inflow <- rtop_pred |>
  filter(
    id %in%
      c(
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

krasn_both <- krasn_inflow |>
  dplyr::left_join(
    sed_data,
    by = dplyr::join_by(id, year)
  ) |>
  dplyr::mutate(
    SSDBoth = ifelse(is.na(ssd_mean), ssd_sim, ssd_mean),
    SSDFlag = ifelse(is.na(ssd_mean), "Sim", "Obs")
  ) |>
  dplyr::mutate(
    bed_f = 28.21 / (95.25 + SSDBoth),
    tot_sd = SSDBoth / (1 - bed_f),
    ssd_tyr = SSDBoth * 31536 / 10^6,
    tot_tyr = tot_sd * 31536 / 10^6
  ) |>
  dplyr::mutate(
    id = dplyr::case_when(
      id == "83174" ~ glue::glue("Kuban (No. {id})"),
      id == "83314" ~ glue::glue("Laba (No. {id})"),
      id == "83361" ~ glue::glue("Belaya (No. {id})"),
      id == "83387" ~ glue::glue("Pshish (No. {id})"),
      TRUE ~ id
    )
  )

# Save
krasn_both |>
  dplyr::transmute(
    Year = year,
    id,
    river = stringr::str_split_i(id, " \\(", i = 1),
    Area = area,
    SSDBoth,
    SSDFlag,
    BedloadF = bed_f,
    SDTotal = SSDBoth / (1 - bed_f)
  ) |>
  qs::qsave(
    "workflow/05_trend/data/ssd_both.qs"
  )

# FIXME:
krasn_inflow_summary <- krasn_both |>
  dplyr::mutate(
    Period = dplyr::case_when(
      dplyr::between(year, 2004, 2015) ~ "2005-2016",
      dplyr::between(year, 2016, 2021) ~ "2016-2021",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(Period)) |>
  dplyr::summarise(
    n = dplyr::n(),
    A = terra::unique(area),
    SSDmean = terra::mean(SSDBoth),
    BFmean = terra::mean(bed_f),
    SSDtot = sum(ssd_tyr),
    SDtot = sum(tot_tyr),
    .by = c(Period, id)
  )

krasn_budget <- krasn_inflow_summary |>
  dplyr::mutate(
    SDtot = dplyr::case_when(
      id == "83361" ~ SDtot * 1,
      TRUE ~ SDtot
    )
  ) |>
  dplyr::mutate(
    SSDtot = dplyr::case_when(
      id == "83361" ~ SSDtot * 1,
      TRUE ~ SSDtot
    )
  ) |>
  dplyr::group_by(Period) |>
  dplyr::summarise(
    FluxSS = sum(SSDtot),
    FluxBS = sum(SDtot) - sum(SSDtot),
    FluxTot = sum(SDtot)
  ) |>
  dplyr::mutate(
    V = dplyr::case_when(
      Period == "2005-2016" ~ 83,
      TRUE ~ 24.66
    ),
    Shore = dplyr::case_when(
      Period == "2005-2016" ~ 0.79,
      TRUE ~ 0.22
    ),
    .after = 1
  ) |>
  dplyr::mutate(
    Income = FluxTot + Shore * 1.7,
    Storage = V * 0.96,
    Error = 100 * (Income - Storage) / Storage
  )

krasn_budget
dplyr::glimpse(krasn_budget)

# Section 4.3 description ------------------------------------------------
krasn_inflow_summary |>
  dplyr::group_by(Period, n) |>
  dplyr::reframe(
    SusIncome = sum(SSDtot),
    BedIncome = sum(SDtot) - sum(SSDtot),
    TotalIncome = sum(SDtot)
  ) |>
  # Estimate mean annual rates
  dplyr::mutate(
    SusRate = SusIncome / n,
    BedRate = BedIncome / n,
    TotalRate = TotalIncome / n
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::where(is.numeric), ~terra::round(.x, 2))
  ) |>
  dplyr::glimpse()

# FIXME:
# rivers_flux is from 04-01_sed-budget-inflow-rates.R
rivers_flux |>
  dplyr::filter(stringr::str_detect(id, "Kuban")) |>
  dplyr::glimpse()

# Pettitt test ------------------------------------------------------------
pett <- krasn_both |>
  dplyr::group_by(id) |>
  tidyr::complete(year = seq(1925, 2021, by = 1)) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |>
  dplyr::mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |>
  tidyr::nest() |>
  dplyr::mutate(
    pt = purrr::map(
      data,
      ~trend::pettitt.test(.x$SSDBoth)$estimate
    )
  ) |>
  dplyr::mutate(
    p = purrr::map(
      data,
      ~trend::pettitt.test(.x$SSDBoth)$p.value
    )
  ) |>
  tidyr::unnest(cols = c(pt, p)) |>
  dplyr::mutate(
    start = purrr::map_dbl(data, ~dplyr::first(.x$year)),
    end = purrr::map_dbl(data, ~dplyr::last(.x$year))
  ) |>
  dplyr::select(-data) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_factor = ifelse(p < 0.05, "sign", "non")) |>
  dplyr::rowwise() |>
  dplyr::mutate(break_year = seq(start, end)[pt]) |>
  dplyr::ungroup()

set.seed(1234)
taylor_res <- krasn_both |>
  group_by(id) |>
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = TRUE) |>
  mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |>
  mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |>
  nest() |>
  mutate(
    taylor = map(
      data,
      ~change_point_analyzer(
        x = .x$SSDBoth,
        labels = .x$year,
        min_candidate_conf = 0.98,
        min_tbl_conf = 0.95,
        n_bootstraps = 5000,
        CI = 0.95
        # method = "CUSUM"
      )
    )
  ) |>
  dplyr::select(-data) |>
  unnest(cols = c(taylor)) |>
  ungroup()

taylor_ci <- taylor_res |>
  mutate(
    label_low = str_split(`CI (95%)`, "-", simplify = TRUE)[, 1],
    label_upper = str_split(`CI (95%)`, "-", simplify = TRUE)[, 2],
  ) |>
  mutate(across(contains("label_"), ~parse_number(.x)))

print(taylor_ci, n = 100)

# CUSUM -------------------------------------------------------------------
krasn_cusum <- krasn_both |>
  group_by(id) |>
  complete(year = seq(1925, 2021, by = 1)) |>
  arrange(year, .by_group = TRUE) |>
  mutate(SSDBoth = imputeTS::na_interpolation(SSDBoth, maxgap = 5)) |>
  mutate(SSDBoth = imputeTS::na_mean(SSDBoth)) |>
  mutate(
    SSDFlag = ifelse(year == "2021", "Sim", SSDFlag)
  ) |>
  ungroup() |>
  select(year, id, SSDBoth, SSDFlag) |>
  group_by(id) |>
  mutate(
    SSDMean = mean(SSDBoth, na.rm = TRUE),
    SSDCv = sd(SSDBoth, na.rm = TRUE) / SSDMean
  ) |>
  mutate(K = SSDBoth / SSDMean - 1) |>
  mutate(CDC = cumsum(K) / SSDCv) |>
  ungroup()

krasn_cusum_plot <- krasn_cusum |>
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
    inherit.aes = FALSE,
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
    ~id,
    scales = "free_y"
  ) +
  labs(
    x = "",
    y = "**CUSUM _SSD_**"
  ) +
  guides(
    fill = guide_legend(
      order = 3,
      ncol = 1,
      title.position = "top"
    )
  )

krasn_cusum_plot

mw_save(
  "figures/fig5_krasnodar-inflow-cusum.png",
  krasn_cusum_plot,
  w = 22,
  h = 13
)

# Create table ----
taylor_pettitt_table <- taylor_ci |>
  transmute(
    id,
    TaylorBreak = glue::glue("{label} ({label_low}–{label_upper})")
  ) |>
  left_join(
    pett |>
      transmute(
        id,
        PettittBreak = glue::glue("{break_year}{ifelse(p < 0.05, '*', '')}")
      ),
    by = join_by(id)
  ) |>
  relocate(PettittBreak, .after = 1)

taylor_pettitt_table

# Save
writexl::write_xlsx(
  taylor_pettitt_table,
  "tables/table7_ssy-break-points.xlsx"
)

# Trends ------------------------------------------------------------------
taylor_periods <- taylor_res |>
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

krasn_period <- krasn_both |>
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
    se = FALSE
  ) +
  scale_y_log10() +
  facet_wrap(~id, scales = "free_y")
