library(tidyverse)
library(sf)
library(here)
library(rmapshaper)
library(patchwork)
library(yardstick)
library(tidyhydro)

source(here::here("R", "funs_rtop.R"))
source(here::here("R", "funs_ggplot2.R"))

ggplot2::theme_set(theme_kbn())

## Metrics
# source("R/funs_utils.R")

val_metrics <- yardstick::metric_set(ccc, rsq, nse)

# 1) Point data ---------------------------------------------------------
kbn_gages <- sf::st_read(
  here::here("data", "vector", "kbn_gages", "kbn_gages.shp"),
  query = "SELECT * FROM kbn_gages WHERE region = 'nw'"
) |>
  dplyr::mutate(id = as.character(id))

# Keep only "pristine" gages
rtop_ids <- kbn_gages |>
  dplyr::filter(is.na(status2)) |>
  dplyr::filter(id != "83361") |> # for validation
  dplyr::pull(id)

north_ids <- kbn_gages |>
  dplyr::pull(id)

# 2) Watersheds -----------------------------------------------------------
ws_ter <- sf::st_read(
  here::here("data/vector/ter_ws-6931/ter_ws-6931.shp"),
  quiet = TRUE
) |>
  dplyr::filter(is.na(status2)) |>
  dplyr::transmute(
    id = label,
    area
  )

ws_all <- sf::st_read(
  here::here("data/vector/kbn_ws_30dec/kbn_ws_30dec.shp"),
  query = "SELECT id, area FROM kbn_ws_30dec",
  quiet = TRUE
) |>
  dplyr::mutate(
    id = as.character(id)
  ) |>
  dplyr::filter(id %in% north_ids) |>
  sf::st_transform(6931) |>
  dplyr::bind_rows(ws_ter) |>
  rmapshaper::ms_simplify(keep = 0.3)

ws_new <- sf::st_read(
  here::here("data/vector/kbn_ws-predict/kbn_ws-predict.shp"),
  query = "SELECT * FROM \"kbn_ws-predict\" WHERE id IS NULL"
) |>
  sf::st_transform(6931) |>
  dplyr::transmute(
    id = river,
    area = a
  ) |>
  dplyr::rename(geometry = "_ogr_geometry_")

# Observed data
ws_obs <- ws_all |>
  dplyr::filter(id %in% rtop_ids | stringr::str_detect(id, "-"))

# Predict to
ws_pred <- ws_all |>
  dplyr::filter(!stringr::str_detect(id, "-")) |>
  dplyr::bind_rows(ws_new)

# Original Gauging station density
region_area_original <- ws_obs |>
  dplyr::filter(!stringr::str_detect(id, "-")) |>
  terra::vect() |>
  terra::aggregate() |>
  terra::expanse("km")

ws_obs |>
  dplyr::filter(!stringr::str_detect(id, "-")) |>
  nrow() |>
  magrittr::divide_by(region_area_original / 10^3)
# > 1.560525

# New Gauging station density
region_area <- ws_obs |>
  terra::vect() |>
  terra::aggregate() |>
  terra::expanse("km")

nrow(ws_obs) / (region_area / 10^3)
# > 1.712814

# 3) Sediment data --------------------------------------------------------
ter_data <- readxl::read_excel(
  here::here("data/hydro/terek_SY.xlsx"),
  sheet = "data_kgs",
  range = "A2:AW96"
) |>
  dplyr::rename(year = 1) |>
  tidyr::gather(id, ssd_mean, -year) |>
  dplyr::relocate(year, .after = id)

sed_data <- qs::qread(
  here::here(
    "workflow",
    "01_sediment-database",
    "data",
    "SSD-yr-all_21dec23.qs"
  )
) |>
  dplyr::mutate(id = as.character(id)) |>
  dplyr::select(id:ssd_mean) |>
  dplyr::bind_rows(
    ter_data
  )

# 4) Merge data -----------------------------------------------------------
ws_obs_sy <- ws_obs |>
  dplyr::left_join(
    sed_data,
    by = dplyr::join_by(id)
  ) |>
  dplyr::mutate(ssy = ssd_mean * 31536 / area) |>
  tidyr::drop_na(ssy)

# BoxCox transformation
# To get back to normal values use
# mutate(.pred = df_new^(1/lamda))
boxcox_l <- forecast::BoxCox.lambda(
  ws_obs_sy$ssy,
  method = "loglik",
  lower = -3,
  upper = 3
)

boxcox_l
# > 0.05

ws_obs_box <- ws_obs_sy |>
  dplyr::mutate(obs = ssy^boxcox_l)

# Viz Miss -----------------------------------------------------------
ssd_miss <- ws_obs_sy |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    Region = ifelse(
      stringr::str_detect(id, "-"),
      "Terek River basin",
      "Krasnodar Reservoir basin"
    )
  ) |>
  dplyr::group_by(Region, id) |>
  dplyr::mutate(ssd_a = data.table::rleid(is.na(ssd_mean))) |>
  dplyr::filter(!is.na(ssd_mean)) |>
  dplyr::group_by(Region, id, ssd_a) |>
  dplyr::summarise(
    start = dplyr::first(year),
    end = dplyr::last(year),
    n = dplyr::n(),
    .groups = "drop"
  )

ssd_miss2 <- ws_obs_sy |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    Region = ifelse(
      stringr::str_detect(id, "-"),
      "Terek River basin",
      "Krasnodar Reservoir basin"
    )
  ) |>
  dplyr::group_by(Region, id) |>
  arrange(year, .by_group = TRUE) |>
  complete(year = seq(min(year), max(year))) |>
  # For vizualizatuion purtposes
  mutate(
    ssd_mean = imputeTS::na_mean(ssd_mean, maxgap = 5)
  ) |>
  dplyr::mutate(
    ssd_a = data.table::rleid(is.na(ssd_mean))
  ) |>
  dplyr::filter(!is.na(ssd_mean)) |>
  dplyr::group_by(Region, id, ssd_a) |>
  dplyr::summarise(
    start = dplyr::first(year),
    end = dplyr::last(year),
    n = dplyr::n(),
    .groups = "drop"
  )

id_order <- ssd_miss2 |>
  dplyr::distinct(Region, id, n) |>
  dplyr::group_by(Region) |>
  dplyr::arrange(dplyr::desc(n), .by_group = TRUE)

id_order |>
  dplyr::count(Region)

# Figure 2
fig02 <- ssd_miss2 |>
  dplyr::filter(id != "83134") |>
  dplyr::mutate(
    id = factor(
      id,
      levels = unique(id_order$id),
      ordered = TRUE
    )
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = start,
      xend = end,
      y = id,
      yend = id
    ),
    linewidth = 1.1
  ) +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(10)
  ) +
  ggplot2::scale_color_manual(
    labels = scales::parse_format()
  ) +
  ggplot2::labs(
    y = "Gauging station ID",
    x = "",
    color = NULL
  ) +
  ggplot2::facet_wrap(
    ~Region,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = "grey90")
  )

mw_save(
  "figures/fig2_data-availability_v1.2.png",
  fig02,
  h = 13,
  w = 21,
  dpi = 500
)

# Gage station description ----
# Gauging station names
## Terek River basin
terek_names <- readxl::read_excel(
  "data/hydro/terek_SY.xlsx",
  sheet = 1,
  range = "A2:B39",
  col_names = c("label", "id")
) |>
  dplyr::mutate(
    label = stringi::stri_trans_general(
      label,
      "russian-latin/bgn"
    ),
    label = stringr::str_replace(
      label,
      "-",
      "—"
    ),
    id = as.character(id)
  )

## Kuban River basin
kuban_names <- readxl::read_excel(
  "data/hydro/gage_id-24082022.xlsx",
  sheet = 1,
  range = "A1:D81"
) |>
  dplyr::transmute(
    label = glue::glue("{river} – {gage}"),
    label = stringi::stri_trans_general(
      label,
      "russian-latin/bgn"
    ),
    id = as.character(id)
  ) |>
  dplyr::mutate(
    label = stringr::str_replace(
      label,
      "Kubanʹ",
      "Kuban"
    )
  )

ws_stats <- ws_obs_sy |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(id, area) |>
  reframe(
    ssy_mean = mean(ssy),
    ssy_median = median(ssy),
    ssy_max = max(ssy),
    ssy_min = min(ssy),
    ssy_sd = sd(ssy)
  )

supp_table <- ssd_miss |>
  filter(n > 1) |>
  left_join(
    ws_stats,
    by = join_by(id)
  ) |>
  mutate(across(where(is.numeric), ~atslib::smart_round(.x))) |>
  left_join(
    bind_rows(
      kuban_names,
      terek_names
    ),
    by = join_by(id)
  ) |>
  relocate(label, .after = id) |>
  relocate(area, .after = label) |>
  mutate(
    period = glue::glue("{start}–{end}"),
    .after = area
  ) |>
  dplyr::select(-ssd_a:-end)

## Save table
writexl::write_xlsx(
  supp_table,
  path = "tables/tableSM_gage-desc.xlsx"
)

# 5) Predict --------------------------------------------------------------
library(rtop)

params <- list(gDist = TRUE, cloud = TRUE)

# 6) Prediction -----------------------------------------------------------
library(furrr)
plan(multisession, workers = 15)

years_available <- ws_obs_box |>
  st_drop_geometry() |>
  count(year, sort = TRUE) |>
  filter(n >= 10) |>
  # filter(year >= 1973) |>
  pull(year) |>
  sort()

set.seed(123)
ssd_uk <- years_available |>
  future_map_dfr(
    ~kbn_rtop(
      ws_obs_box,
      .year = .x,
      .iter = 2000,
      .boxcox = boxcox_l
    ),
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE) # !!!!!
  )

# CV metrics --------------------------------------------------------------
ssd_uk |>
  unnest(c(.cv_res)) |>
  ggdist::mean_qi(CCC)

ssd_uk_cv <- ssd_uk |>
  select(.cv_df) |>
  unnest(c(.cv_df))

ssd_uk_cv |>
  yardstick::ccc(truth = obs, estimate = var1.pred)

hydroGOF::NSE(ssd_uk_cv$var1.pred, ssd_uk_cv$obs)

# Find rtop outliers based on Cook's Distance

cv_df <- ssd_uk |>
  select(.cv_df) |>
  unnest(c(.cv_df))

mod <- lm(var1.pred ~ obs - 1, data = cv_df)

cooksd <- cooks.distance(mod)

# cutoff <- 4 / length(cooksd)
cutoff <- 1 * mean(cooksd)

cutoff_df <- cv_df[cooksd > cutoff, ]
# bind_rows(
#   filter(
#     cv_df,
#     id == "83340" & year == 2012 |
#       id == "83216" & year == 1966 |
#       id == "83395" & year == 2016 |
#       id == "83437" & year == 2015 |
#       id == "83207" & year == 2011 |
#       id == "83395" & year == 2019 |
#       id == "83424" & year == 1957 |
#       id == "83437" & year == 1969 |
#       id == "83385" & year == 1980 |
#       id == "83262" & year == 2006
#   )
# )

cv1_plot <- ssd_uk |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  ggplot(
    aes(
      x = obs,
      y = var1.pred
    )
  ) +
  geom_abline(
    linetype = "dashed"
  ) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
    formula = y ~ x
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1
  ) +
  # geom_text(
  ggrepel::geom_text_repel(
    aes(
      label = paste0(id, "\n", year)
    ),
    size = 3
  ) +
  geom_point(
    data = cutoff_df,
    aes(
      x = obs,
      y = var1.pred
    ),
    color = "firebrick"
  ) +
  tune::coord_obs_pred()

cv1_plot

# Repeat CV without influential catchment-years

set.seed(123)
ssd_uk2 <- years_available |>
  future_map_dfr(
    ~kbn_rtop(
      ws_obs_box,
      .year = .x,
      .iter = 2000,
      .boxcox = boxcox_l,
      .cooks_cutoff = TRUE
    ),
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE) # !!!!!
  )

ssd_uk2 |>
  unnest(c(.cv_res)) |>
  ggdist::median_qi(CCC)

ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  # filter(id != "83216") |>
  yardstick::ccc(truth = obs, estimate = var1.pred)

ssd_uk2_cv <- ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df))

cv_metrics <- ssd_uk2_cv |>
  val_metrics(obs, var1.pred) |>
  transmute(
    .metric = c("CCC", "R^2", "NSE"),
    .metric = paste0("bold(", .metric, ")"),
    .estimate = round(.estimate, 2)
  )

cv_scatter <- ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  mutate(facet = "(a) Cross-Validation") |>
  ggplot(
    aes(
      x = obs,
      y = var1.pred
    )
  ) +
  geom_abline(linetype = "dashed") +
  geom_point(alpha = 0.25) +
  ggpp::geom_table_npc(
    data = cv_metrics,
    label = list(cv_metrics),
    npcx = 0.95,
    npcy = 0.05,
    parse = TRUE,
    table.colnames = FALSE,
    family = "Merriweather",
    table.theme = ttheme_gtminimal
  ) +
  lims(
    x = c(1.1, 1.55),
    y = c(1.1, 1.55)
  ) +
  labs(
    x = "**Observed _SSD_**",
    y = "**Predicted _SSD_**"
  ) +
  facet_wrap(~facet)

cv_hist <- ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  mutate(facet = "(c) Cross-Validation") |>
  ggplot(
    aes(
      x = obs - var1.pred
    )
  ) +
  geom_density(
    aes(y = ..scaled..),
    fill = "grey70"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(5),
    limits = c(-0.11, 0.11),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_extended(n = 5),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    x = "**Cross-validation residuals**",
    y = ""
  ) +
  facet_wrap(~facet) +
  theme(
    panel.grid.major.y = element_blank()
  )

cv_plot <- cv_scatter + cv_hist

cv_plot

mw_save(
  here("workflow/02_rtop-interpolation/figures/cv_qa.png"),
  cv_plot,
  w = 20,
  h = 12
)

# Save --------------------------------------------------------------------
# qs::qsave(
# ssd_uk2,
# here("workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs")
# )

# Krasnodar gage validation -----------------------------------------------
library(ggpmisc)

ssd_uk2 <- qs::qread(
  here("workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs")
)

krasnodar_tyr <- sed_data |>
  filter(id == "83183") |>
  mutate(
    ssd_tyr = ssd_mean * 31536 / 10^6
  ) |>
  drop_na(ssd_mean)

model_area <- ws_pred |>
  filter(
    id %in%
      c(
        "Psekups",
        "Shunduk",
        "Apchas",
        "Marta",
        "83387",
        "83361",
        "83314",
        "83174",
        "83413"
      )
  ) |>
  terra::vect() |>
  terra::aggregate() |>
  terra::expanse("km")

krasnodar_db <- ssd_uk2$.pred_df |>
  bind_rows() |>
  as_tibble() |>
  filter(
    id %in%
      c(
        "Psekups",
        "Shunduk",
        "Apchas",
        "Marta",
        "83387",
        "83361",
        "83314",
        "83174",
        "83413"
      )
  ) |>
  mutate(
    ssd_mean = var1.pred^(1 / boxcox_l),
    ssd_sim = ssd_mean * area / 31536,
    ssd_tyr = ssd_sim * 31536 / 10^6
  )

krasnodar_db_tot <- krasnodar_db |>
  group_by(year) |>
  reframe(
    upstream_tyr = sum(ssd_tyr)
  ) |>
  # Adjust to bigger area
  mutate(
    upstream_ssy = 10^6 * upstream_tyr / model_area,
    upstream_tyr_new = upstream_ssy * 45900 / 10^6
  )

krasnodar_tyr |>
  filter(year < 1945) |>
  # filter(year > 1945) |>
  left_join(
    krasnodar_db_tot,
    by = join_by(year)
  ) |>
  mutate(
    rel = 100 * (ssd_tyr - upstream_tyr_new) / ssd_tyr
  ) |>
  ggdist::mean_qi(rel)
# > 31.2 %

krasnodar_metrics <- krasnodar_tyr |>
  filter(year < 1945) |>
  # filter(year > 1945) |>
  left_join(
    krasnodar_db_tot,
    by = join_by(year)
  ) |>
  mutate(
    across(
      c(ssd_tyr, upstream_tyr_new),
      ~{
        .x^0.05
      }
    )
  ) |>
  val_metrics(ssd_tyr, upstream_tyr_new) |>
  transmute(
    .metric = c("CCC", "R^2", "NSE"),
    .metric = paste0("bold(", .metric, ")"),
    .estimate = round(.estimate, 2)
  )

val_scatter <- krasnodar_tyr |>
  # filter(year > 1945 & year < 1973) |>
  filter(year < 1945) |>
  left_join(krasnodar_db_tot, by = join_by(year)) |>
  mutate(facet = "(b) g/s Krasnodar City") |>
  ggplot(
    aes(
      y = (upstream_tyr_new)^0.05,
      x = (ssd_tyr)^0.05
    )
  ) +
  stat_poly_line(color = "firebrick4") +
  stat_poly_eq(use_label("eq"), family = "Merriweather") +
  geom_abline(linetype = "dashed") +
  geom_point(alpha = 0.65) +
  ggpp::geom_table_npc(
    data = krasnodar_metrics,
    label = list(krasnodar_metrics),
    npcx = 0.95,
    npcy = 0.05,
    parse = TRUE,
    table.colnames = FALSE,
    family = "Merriweather",
    table.theme = ttheme_gtminimal
  ) +
  ggrepel::geom_text_repel(
    aes(label = year),
    family = "Merriweather",
    size = 3,
  ) +
  lims(
    x = c(1.05, 1.15),
    y = c(1.05, 1.15)
  ) +
  labs(
    x = "**Observed _SSD_**",
    y = "**Predicted _SSD_**"
  ) +
  facet_wrap(~facet)

val_scatter

val_hist <- krasnodar_tyr |>
  # filter(year > 1945 & year < 1973) |>
  filter(year < 1945) |>
  left_join(
    krasnodar_db_tot,
    by = join_by(year)
  ) |>
  mutate(facet = "(d) g/s Krasnodar City") |>
  ggplot(
    aes(
      x = (ssd_tyr)^0.05 - (upstream_tyr_new)^0.05
    )
  ) +
  geom_density(
    aes(y = ..scaled..),
    fill = "grey70"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(5),
    limits = c(-0.02, 0.07),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_extended(n = 5),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    x = "**Validation residuals**",
    y = ""
  ) +
  facet_wrap(~facet) +
  theme(
    panel.grid.major.y = element_blank()
  )

val_hist

# cv_val_plot <-
#   (cv_scatter + cv_hist +
#      plot_layout(
#        widths = c(2, 1)
#      )) /
#   (val_scatter + val_hist +
#      plot_layout(
#        widths = c(2, 1)
#      ))
#
# cv_val_plot

cv_val_plot <- cv_scatter + val_scatter + (cv_hist / val_hist)

cv_val_plot

mw_save(
  here("workflow/02_rtop-interpolation/figures/cv_qa.png"),
  cv_val_plot,
  w = 25,
  h = 10
)

# How much is the model underestimates? -----------------------------------
val_eq <- \(y) (y - 0.174) / 0.859

data.frame(
  obs = seq(1.045, 1.130, length.out = 100)
) |>
  mutate(
    sim = val_eq(obs),
    sim = sim^(1 / 0.05),
    obs = obs^(1 / 0.05),
    error = 100 * (sim - obs) / obs
  ) |>
  reframe(
    mean(error)
  )

# Validation --------------------------------------------------------------
ssd_uk2$.pred_df |>
  bind_rows() |>
  filter(id == "83361") |>
  mutate(
    ssd_sim = var1.pred^(1 / boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |>
  left_join(
    sed_data,
    by = join_by(
      id,
      year
    )
  ) |>
  ggplot(
    aes(
      x = log10(ssd_mean),
      y = log10(ssd_sim)
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x - 1
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1
  ) +
  tune::coord_obs_pred()

# Check other gages -------------------------------------------------------
ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(
    id %in% c("83395", "83387", "83361", "83314", "83174")
  ) |>
  mutate(
    ssd_sim = var1.pred^(1 / boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |>
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |>
  select(id, year, obs = ssd_mean, ssd_sim) |>
  ggplot(
    aes(
      y = ssd_sim^boxcox_l,
      x = obs^boxcox_l
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x - 1
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1
  ) +
  # scale_x_log10() +
  # scale_y_log10() +
  tune::coord_obs_pred() +
  facet_wrap(~id)

# Timeseries
ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(id == "83265") |>
  mutate(
    ssd_sim = var1.pred^(1 / boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |>
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |>
  select(year, ssd_mean, ssd_sim) |>
  gather(type, ssd, -year) |>
  ggplot(
    aes(
      x = year,
      y = ssd,
      color = type
    )
  ) +
  geom_line() +
  geom_point(
    data = . %>% filter(type == "ssd_mean"),
    shape = 21
  )

ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(
    id %in%
      c(
        "Psekups",
        "Shunduk",
        "Apchas",
        "Marta",
        "83387",
        "83361",
        "83314",
        "83174",
        "83413",
        "83307"
      )
  ) |>
  mutate(
    ssd_sim = var1.pred^(1 / boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |>
  group_by(id) |>
  rstatix::get_summary_stats(ssd_sim, type = "robust")
select(-var1.var:-sumWeights) |>
  mutate(f_bedload = 0.525 - 0.0506 * log(area)) |>
  mutate(f_bedload2 = 0.55 - 0.04 * log(area)) |>
  count(id, area, f_bedload2)

# 7) Save -----------------------------------------------------------------
# qs::qsave(
#   ssd_uk,
#   here("workflow", "02_rtop-interpolation", "data", "rtop_2000.qs")
# )

# 8) Compare --------------------------------------------------------------
ssd_uk <- qs::qread(
  here("workflow", "02_rtop-interpolation", "data", "rtop_2000.qs")
)

library(yardstick)

ws_pred_qa <- ws_pred |>
  left_join(
    sed_data,
    by = join_by(id)
  ) |>
  mutate(
    ssy = ssd_mean * 31536 / area,
    obs = ssy^boxcox_l
  ) |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(
    ssd_uk |>
      select(id, year, var1.pred, var1.var),
    by = join_by(id, year)
  )

ws_obs_qa <- ws_obs_box |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(
    ssd_uk,
    by = join_by(id, year)
  )

ws_pred_qa |>
  filter(!id %in% rtop_ids) |>
  drop_na(obs) |>
  count(id, sort = TRUE)

ws_pred_qa |>
  # filter(!id %in% rtop_ids) |>
  filter(id == "83314") |>
  ggplot(
    aes(
      x = obs,
      y = var1.pred
    )
  ) +
  geom_abline(
    linetype = "dashed"
  ) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x
  ) +
  tune::coord_obs_pred()

ws_pred_qa |>
  # filter(!id %in% rtop_ids) |>
  filter(id == "83174") |>
  select(year, obs, var1.pred) |>
  gather(type, ssy_box, -year) |>
  ggplot(
    aes(
      x = year,
      y = ssy_box,
      color = type
    )
  ) +
  geom_line() +
  geom_point(
    shape = 21
  )

ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(id == "83395") |>
  mutate(var1.pred = (var1.pred / 1.22)^(1 / boxcox_l)) |>
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |>
  select(year, obs = ssd_mean, var1.pred) |>
  gather(type, ssy_box, -year) |>
  ggplot(
    aes(
      x = year,
      y = ssy_box,
      color = type
    )
  ) +
  geom_line() +
  geom_point(
    shape = 21
  )

ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(!str_detect(id, "-")) |>
  filter(id == "83137") |>
  # mutate(var1.pred = var1.pred^(1/boxcox_l)) |>
  left_join(
    sed_data |>
      mutate(ssd_mean = ssd_mean^boxcox_l),
    by = join_by(id, year)
  ) |>
  select(year, obs = ssd_mean, var1.pred) |>
  ggplot(
    aes(
      y = var1.pred,
      x = obs
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  tune::coord_obs_pred()

ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  # filter(id == "83137") |>
  ggplot(
    aes(
      obs,
      var1.pred
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  tune::coord_obs_pred()

ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  filter(id == "83137")

pred_df <- ssd_uk2 |>
  select(.pred_df) |>
  unnest(c(.pred_df)) |>
  select(id, year, pred = var1.pred)

cv_df <- ssd_uk2 |>
  select(.cv_df) |>
  unnest(c(.cv_df)) |>
  select(id, year, cv = var1.pred)

cv_df |>
  left_join(pred_df, by = join_by(id, year)) |>
  ggplot(aes(x = pred, y = cv)) +
  geom_abline(slope = 1) +
  geom_point() +
  tune::coord_obs_pred()
