library(here)
library(tidyverse)

source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

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
    ssd_sim = var1.pred^(1 / 0.05), # Mind the Box-Cox lambda
    ssd_sim = ssd_sim * area / 31536,
    bed_f = 28.21 / (95.25 + ssd_sim), # see summary(bedload_model)
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

# Cumulative inflow -------------------------------------------------------
plot_sediments <-
  krasn_inflow |>
  filter(year >= 2004) |>
  mutate(tot_tyr = ifelse(
    id == "83361",
    tot_tyr / 2,
    tot_tyr
  )) |>
  mutate(
    group = ifelse(
      id %in% c("83174", "83314", "83361"),
      "(c) North-East tributaries",
      "(d) South tributaries"
    )
  ) |>
  ggplot(
    aes(
      x = year,
      y = tot_tyr,
      color = id
    )
  ) +
  geom_line(
    aes(lty = id),
    lwd = 1.25,
    key_glyph = draw_key_path
  ) +
  scale_y_continuous(
    name = "**Total sediment load [t/yr]**",
    breaks = scales::pretty_breaks(n = 5),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = "",
    breaks = scales::pretty_breaks(n = 7),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(
    name = "",
    values = clrs,
    labels = c(
      "Kuban (No. 83174)", "Laba (No. 83314)", "Belaya (No. 83361)",
      "Pshish (No. 83387)", "Apchas", "Marta", "Psekups", "Shunduk"
    ),
  ) +
  scale_linetype_manual(
    name = "",
    values = rep(c("solid", "32"), 4),
    labels = c(
      "Kuban (No. 83174)", "Laba (No. 83314)", "Belaya (No. 83361)",
      "Pshish (No. 83387)", "Apchas", "Marta", "Psekups", "Shunduk"
    ),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  facet_wrap(~group, scales = "free_y", nrow = 2) +
  theme(
    legend.justification = "left",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.spacing = unit(0.3, "lines"),
    legend.background = element_blank(),
    plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5.5),
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    )
  )

plot_sediments

# Sediment Budget ---------------------------------------------------------
# !NB
# krasn_inflow_summary is created in 
# 05-01_cusum-analysis.R

# krasn_inflow_summary <-
#   krasn_inflow |>
#   mutate(
#     Period = case_when(
#       between(year, 2004, 2015) ~ "2005-2016",
#       between(year, 2016, 2021) ~ "2016-2021",
#       TRUE ~ NA_character_
#     )
#   ) |>
#   filter(!is.na(Period)) |>
#   summarise(
#     A = unique(area),
#     SSDmean = mean(ssd_sim),
#     BFmean = mean(bed_f),
#     SSDtot = sum(ssd_tyr),
#     SDtot = sum(tot_tyr),
#     .by = c(Period, id)
#   )

krasn_budget <-
  krasn_inflow_summary |>
  mutate(
    SDtot = case_when(
      id == "83361" ~ SDtot * 0.5,
      TRUE ~ SDtot
    )
  ) |>
  mutate(
    SSDtot = case_when(
      id == "83361" ~ SSDtot * 0.5,
      TRUE ~ SSDtot
    )
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
    .after = 1
  ) |>
  mutate(
    VolSS = FluxSS / 0.96,
    VolBS = FluxBS / 1.7,
    # s_vol = s_flux / 1.3
    VolS = VolSS + VolBS + Shore,
    Error = 100 * (VolS - V) / V
  )

krasn_budget

# Budget per rivers
raw_flux <-
  krasn_inflow_summary |>
  mutate(
    SDtot = case_when(
      id == "83361" ~ SDtot * 0.5,
      TRUE ~ SDtot
    )
  ) |>
  mutate(
    SSDtot = case_when(
      id == "83361" ~ SSDtot * 0.5,
      TRUE ~ SSDtot
    )
  ) |>
  group_by(Period) |>
  mutate(
    FluxBS = sum(SDtot - SSDtot),
    FluxSS = sum(SSDtot),
    FluxTot = sum(SDtot)
  ) |>
  ungroup() |>
  mutate(
    SSFlux = SSDtot,
    BSFlux = SDtot - SSDtot,
    TotFlux = SDtot,
    SSPct = SSDtot / FluxSS,
    BSPct = (SDtot - SSDtot) / FluxBS,
    TotPct = SDtot / FluxTot
  )

krasn_inflow |>
  mutate(
    Period = case_when(
      between(year, 2004, 2015) ~ "2005-2016",
      between(year, 2016, 2021) ~ "2016-2021",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    tot_tyr = case_when(
      id == "83361" ~ tot_tyr * 0.5,
      TRUE ~ tot_tyr
    )
  ) |>
  filter(!is.na(Period)) |>
  group_by(Period, year) |>
  reframe(TotFlux = sum(tot_tyr)) |>
  group_by(Period) |>
  ggdist::mean_qi(TotFlux)

rivers_flux <-
  raw_flux |>
  group_by(Period) |>
  nest() |>
  mutate(data = map(data, ~ janitor::adorn_totals(.x))) |>
  unnest(cols = c(data)) |>
  mutate(
    across(ends_with("Pct"), ~ round(100 * .x, 2)),
    across(ends_with("Flux"), ~ round(.x, 2))
  ) |>
  transmute(
    Period, id,
    SSFlux = glue::glue("{SSFlux} ({SSPct} %)"),
    BSFlux = glue::glue("{BSFlux} ({BSPct} %)"),
    TotFlux = glue::glue("{TotFlux} ({TotPct} %)"),
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

rivers_flux

# Budget plot
plot_krasn_budget <-
  krasn_budget |>
  select(Period, V, Shore, FluxSS, FluxBS) |>
  gather(
    part, role, -Period, -V
  ) |>
  mutate(
    part = factor(part, levels = c("Shore", "FluxBS", "FluxSS"))
  ) |>
  group_by(Period) |>
  mutate(pct = role / sum(role)) |>
  mutate(facet = "(b) Sediment budget") |>
  ggplot(
    aes(
      x = Period,
      y = pct,
      fill = part
    )
  ) +
  geom_col(
    width = 0.5,
    position = position_dodge(
      width = 0.6
    ),
    key_glyph = draw_key_rect
  ) +
  geom_text(
    aes(label = scales::percent(pct, 0.1)),
    position = position_dodge(
      width = 0.6
    ),
    family = "Merriweather",
    size = rel(2.5),
    vjust = -0.15
  ) +
  scale_y_continuous(
    name = "**Component role**",
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = clrs[c(1, 4, 7)],
    labels = c(
      "Shore abrasion",
      "Bedload sediments",
      "Suspsended sediments"
    )
  ) +
  labs(
    fill = "",
    x = ""
  ) +
  facet_wrap(~facet) +
  theme(
    legend.justification = "left",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1, "lines"),
    legend.spacing = unit(0.3, "lines"),
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    ),
    legend.background = element_blank()
  )

plot_krasn_budget

# Sedimentation/Denudation ------------------------------------------------
sed_den <-
  readxl::read_excel(
    "data/tables/krasnodar_res_vol.xlsx",
    range = "A1:N7"
  ) |>
  filter(!is.na(time2)) |>
  transmute(
    time1, time2,
    Period = paste0(time1, "-", time2),
    E = `denudation rate, mm/yr`,
    S = `sedimentation rate, %`
  ) |>
  mutate(across(c(time1, time2), ~ as.numeric(.x))) |>
  mutate(Period = as_factor(Period))

sed_den |>
  ggplot() +
  geom_rect(
    aes(
      xmin = time1, xmax = time2,
      ymin = 0, ymax = S
    ),
    fill = colorspace::lighten(clrs[7], amount = 0.3),
    color = "black"
  ) +
  geom_text(
    aes(
      x = (time1 + time2) / 2,
      y = S,
      label = scales::percent(S, scale = 1, accuracy = 0.01)
    ),
    family = "Merriweather",
    fontface = "bold",
    vjust = -0.25,
    size = 3.5
  ) +
  geom_text(
    aes(
      x = (time1 + time2) / 2,
      y = S,
      label = paste0(round(E, 2), " mm")
    ),
    family = "Merriweather",
    fontface = "bold",
    color = "white",
    vjust = 1.25,
    size = 3
  ) +
  scale_x_continuous(
    breaks = unique(c(sed_den$time1, sed_den$time2)),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = "**Sedimentation rate _S_ [%]**",
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(~ . / (max(sed_den$S) / max(sed_den$E)),
      name = "**Denudation rate _E_ [mm/yr]**"
    )
  )

plot_sed <-
  sed_den |>
  mutate(
    facet = "(a) Sedimentation & denudation"
  ) |>
  ggplot() +
  geom_line(
    aes(
      x = Period,
      y = S,
      color = "Sedimentation",
      group = NA
    ),
    lty = "32",
    lwd = 1.2
  ) +
  geom_line(
    aes(
      x = Period,
      y = E * (max(S) / max(E)),
      color = "Denudation",
      group = NA
    ),
    lwd = 1.2
  ) +
  scale_x_discrete(
    name = "",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = "**Sedimentation rate _S_ [%]**",
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / (max(sed_den$S) / max(sed_den$E)),
      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5) / (max(sed_den$S) / max(sed_den$E)),
      labels = scales::label_number(accuracy = 0.01),
      name = "**Denudation rate _E_ [mm/yr]**"
    )
  ) +
  scale_color_manual(
    name = "",
    values = clrs[c(7, 2)],
    breaks = c("Sedimentation", "Denudation"),
    guide = guide_legend(
      nrow = 2
    )
  ) +
  facet_wrap(~facet) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.15),
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    ),
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.background = element_blank(),
    plot.background = element_blank()
  )

plot_sed

# Average values ----
sed_den |>
  select(E, S, Period) |>
  gather(var, val, -Period) |>
  group_by(var) |>
  rstatix::get_summary_stats(type = "common")

# Save ---------------------------------------------------------------
# Tables
library(writexl)

krasn_budget |>
  writexl::write_xlsx("tables/table5_krasnodar-reservoir-sediment-budget.xlsx")

rivers_flux |>
  writexl::write_xlsx("tables/table6_krasnodar-reservoir-river-flux.xlsx")

# Figures
library(patchwork)

reservoir_plot <-
  (plot_krasn_budget | plot_sed) /
  plot_sediments +
  plot_layout(
    heights = c(1, 1.65)
  )

reservoir_plot2 <-
  (plot_sed / plot_krasn_budget)

reservoir_plot2
# Saved to SVG via httpgd


ggmw::mw_save(
  "figures/fig3_reservoir-sedimentation-budget.png",
  reservoir_plot,
  w = 22,
  h = 15.7
)
