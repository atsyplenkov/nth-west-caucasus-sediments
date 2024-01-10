library(tidyverse)
library(sf)
library(here)
library(rmapshaper)

source(here("R", "funs_rtop.R"))
source(here("R", "funs_ggplot2.R"))

theme_set(theme_kbn())

# 1) Point data ---------------------------------------------------------
kbn_gages <- 
  st_read(
    here("data", "vector", "kbn_gages", "kbn_gages.shp"),
    query = "SELECT * FROM kbn_gages WHERE region = 'nw'"
  ) |> 
  mutate(id = as.character(id))

# Keep only "pristine" gages
rtop_ids <- 
  kbn_gages |>
  filter(is.na(status2)) |>
  filter(id != "83361") |> # for validation
  pull(id)

north_ids <- 
  kbn_gages |> 
  pull(id)

# 2) Watersheds -----------------------------------------------------------
ws_ter <- 
  st_read(
    here("data/vector/ter_ws-6931/ter_ws-6931.shp")
  ) |> 
  filter(is.na(status2)) |> 
  transmute(
    id = label,
    area
  )

ws_all <- 
  st_read(
    here("data/vector/kbn_ws_30dec/kbn_ws_30dec.shp"),
    query = "SELECT id, area FROM kbn_ws_30dec"
  ) |> 
  mutate(
    id = as.character(id)
  ) |> 
  filter(id %in% north_ids) |>
  st_transform(6931) |> 
  bind_rows(ws_ter) |> 
  ms_simplify(keep = 0.3)

ws_new <- 
  st_read(
    here("data/vector/kbn_ws-predict/kbn_ws-predict.shp"),
    query = "SELECT * FROM \"kbn_ws-predict\" WHERE id IS NULL"
  ) |> 
  st_transform(6931) |> 
  transmute(id = river,
            area = a) |> 
  rename(geometry = "_ogr_geometry_")

# Observed data
ws_obs <- 
  ws_all |> 
  filter(id %in% rtop_ids | str_detect(id, "-"))

# Predict to
ws_pred <- 
  ws_all |> 
  filter(!str_detect(id, "-")) |> 
  bind_rows(ws_new)

# Gauging station density
region_area <- 
  ws_obs |> 
  ms_dissolve() |> 
  st_area() |> 
  magrittr::divide_by(10^6) 

nrow(ws_obs) / (region_area / 10^3)

# 3) Sediment data --------------------------------------------------------
ter_data <-
  readxl::read_excel(
    here("data/hydro/terek_SY.xlsx"),
    sheet = "data_kgs",
    range = "A2:AW96"
  ) |> 
  rename(year = 1) |> 
  gather(id, ssd_mean, -year) |> 
  relocate(year, .after = id) 

sed_data <- 
  qs::qread(
    here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs")
  ) |> 
  mutate(id = as.character(id)) |> 
  select(id:ssd_mean) |> 
  bind_rows(
    ter_data
  )

# 4) Merge data -----------------------------------------------------------
ws_obs_sy <- 
  ws_obs |> 
  left_join(
    sed_data,
    by = join_by(id)
  ) |> 
  mutate(ssy = ssd_mean * 31536 / area) |> 
  drop_na(ssy)

# BoxCox transformation
# To get back to normal values use
# mutate(.pred = df_new^(1/lamda))
boxcox_l <- 
  forecast::BoxCox.lambda(ws_obs_sy$ssy,
                          method = "loglik",
                          lower = -3,
                          upper = 3)

boxcox_l

ws_obs_box <- 
  ws_obs_sy |> 
  mutate(obs = ssy^boxcox_l)

# 5) Predict --------------------------------------------------------------
library(rtop)

params  <- 
  list(gDist = TRUE, cloud = TRUE)

# 6) Prediction -----------------------------------------------------------
library(furrr)
plan(multisession, workers = 15)

years_available <-
  ws_obs_box |> 
  st_drop_geometry() |> 
  count(year, sort = T) |> 
  filter(n >= 10) |>
  # filter(year >= 1973) |> 
  pull(year) |> 
  sort()

set.seed(123)
ssd_uk <- 
  years_available |> 
  future_map_dfr(
    ~kbn_rtop(
      ws_obs_box, 
      .year = .x, 
      .iter = 2000, 
      .boxcox = boxcox_l),
    .progress = TRUE, 
    .options = furrr::furrr_options(seed = TRUE) # !!!!!
  )

# CV metrics --------------------------------------------------------------
ssd_uk |> 
  unnest(c(.cv_res)) |> 
  ggdist::mean_qi(CCC)

ssd_uk_cv <- 
  ssd_uk |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) 

ssd_uk_cv |> 
  yardstick::ccc(truth = obs, estimate = var1.pred)

hydroGOF::NSE(ssd_uk_cv$var1.pred, ssd_uk_cv$obs)

# Find rtop outliers based on Cook's Distance

cv_df <- 
  ssd_uk |>
  select(.cv_df) |> 
  unnest(c(.cv_df))

mod <-  
  lm(var1.pred ~ obs - 1, data = cv_df)

cooksd <- 
  cooks.distance(mod)

# cutoff <- 4 / length(cooksd)
cutoff <- 1 * mean(cooksd)

cutoff_df <- 
  cv_df[cooksd > cutoff, ]
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

cv1_plot <- 
  ssd_uk |>
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
    aes(label =  paste(stat(eq.label),
                       stat(rr.label),
                       sep = "~~~~")),
    formula = y ~ x
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1 
  ) +
  # geom_text(
  ggrepel::geom_text_repel(
    aes(
      label = paste0(id, "\n",year)
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
ssd_uk2 <- 
  years_available |> 
  future_map_dfr(
    ~kbn_rtop(
      ws_obs_box, 
      .year = .x, 
      .iter = 2000, 
      .boxcox = boxcox_l,
      .cooks_cutoff = T
    ),
    .progress = TRUE, 
    .options = furrr::furrr_options(seed = TRUE) # !!!!!
  )

ssd_uk2 |> 
  unnest(c(.cv_res)) |> 
  ggdist::mean_qi(CCC)

ssd_uk2 |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) |> 
  # filter(id != "83216") |> 
  yardstick::ccc(truth = obs, estimate = var1.pred)

ssd_uk2_cv <- 
  ssd_uk2 |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) 

ssd_uk2_cv |> 
  yardstick::ccc(truth = obs, estimate = var1.pred)

ssd_uk2_cv |> 
  yardstick::rsq(truth = obs, estimate = var1.pred)

hydroGOF::NSE(ssd_uk2_cv$var1.pred, ssd_uk2_cv$obs)

cv_scatter <- 
  ssd_uk2 |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) |> 
  mutate(
    facet = "(a) CV scatterplot"
  ) |> 
  ggplot(
    aes(
      x = obs,
      y = var1.pred
    )
  ) +
  geom_abline(
    linetype = "dashed" 
  ) +
  geom_point(
    alpha = 0.25
  ) +
  tune::coord_obs_pred() +
  labs(
    x = "**Observed _SSD_**",
    y = "**Predicted _SSD_**"
  ) +
  facet_wrap(~facet)

cv_hist <- 
  ssd_uk2 |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) |>
  mutate(facet = "(b) CV residuals") |> 
  ggplot(
    aes(
      x = obs - var1.pred
    )
  ) +
  geom_histogram(
    bins = 21
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(5),
    limits = c(-0.11, 0.11),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "**Cross-validation residuals**",
    y = "**Count**"
  ) +
  facet_wrap(~facet)

library(patchwork)

cv_plot <- 
  cv_scatter + cv_hist

ggmw::mw_save(
  here("workflow/02_rtop-interpolation/figures/cv_qa.png"),
  cv_plot,
  w = 16,
  h = 8
)

# Save --------------------------------------------------------------------
qs::qsave(
  ssd_uk2,
  here("workflow/02_rtop-interpolation/data/rtop_cv_7jan23.qs")
)

# Krasnodar gage validation -----------------------------------------------
krasnodar_tyr <- 
  sed_data |> 
  filter(id == "83183") |> 
  mutate(
    ssd_tyr = ssd_mean * 31536 / 10^6
  ) |> 
  drop_na(ssd_mean)

krasnodar_db <- 
  ssd_uk2$.pred_df |> 
  bind_rows() |> 
  as_tibble() |> 
  filter(
    id %in% c(
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
    ssd_mean = var1.pred^(1/boxcox_l),
    ssd_sim = ssd_mean * area / 31536,
    ssd_tyr = ssd_sim * 31536 / 10^6
  )

krasnodar_db_tot <- 
  krasnodar_db |> 
  group_by(year) |> 
  reframe(
    upstream_tyr = sum(ssd_tyr)
  ) 

krasnodar_tyr |>
  filter(year < 1974) |> 
  filter(year > 1945) |>
  left_join(
    krasnodar_db_tot,
    by = join_by(year)
  ) |> 
  ggplot(
    aes(
      y = log10(upstream_tyr),
      x = log10(ssd_tyr)
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  ggrepel::geom_text_repel(
    aes(
      label = year
    )
  ) +
  ggpmisc::stat_poly_eq(
    aes(label =  paste(stat(eq.label),
                       stat(adj.rr.label),
                       sep = "~~~~")),
    formula = y ~ x 
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x
  ) +
  tune::coord_obs_pred()

df <- 
  krasnodar_tyr |>
  filter(year < 1974) |> 
  filter(year > 1945) |>
  left_join(
    krasnodar_db_tot,
    by = join_by(year)
  )

hydroGOF::NSE(
  sim = log10(df$upstream_tyr), 
  obs = log10(df$ssd_tyr)
)

# Validation --------------------------------------------------------------
ssd_uk2$.pred_df |> 
  bind_rows() |> 
  filter(id == "83361") |> 
  mutate(
    ssd_sim = var1.pred^(1/boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |> 
  left_join(
    sed_data,
    by = join_by(
      id, year
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
    aes(label =  paste(stat(eq.label),
                       stat(adj.rr.label),
                       sep = "~~~~")),
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
    ssd_sim = var1.pred^(1/boxcox_l),
    ssd_sim = ssd_sim * area / 31536
  ) |> 
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |> 
  select(id, year, obs=ssd_mean, ssd_sim) |> 
  ggplot(
    aes(
      y = ssd_sim^boxcox_l,
      x = obs^boxcox_l
    )
  ) +
  geom_abline(slope = 1) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label =  paste(stat(eq.label),
                       stat(adj.rr.label),
                       sep = "~~~~")),
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
    ssd_sim = var1.pred^(1/boxcox_l),
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
      color  = type
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
    id %in% c(
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
  )|> 
  mutate(
    ssd_sim = var1.pred^(1/boxcox_l),
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
ssd_uk <- 
  qs::qread(
    here("workflow", "02_rtop-interpolation", "data", "rtop_2000.qs")
  )

library(yardstick)

ws_pred_qa <- 
  ws_pred |> 
  left_join(
    sed_data,
    by = join_by(id)
  ) |> 
  mutate(ssy = ssd_mean * 31536 / area,
         obs = ssy^boxcox_l) |> 
  st_drop_geometry() |>
  as_tibble() |> 
  left_join(
    ssd_uk |> 
      select(id, year, var1.pred, var1.var),
    by = join_by(id, year)
  )

ws_obs_qa <- 
  ws_obs_box |> 
  st_drop_geometry() |>
  as_tibble() |> 
  left_join(
    ssd_uk,
    by = join_by(id, year)
  )

ws_pred_qa |> 
  filter(!id %in% rtop_ids) |> 
  drop_na(obs) |> 
  count(id, sort = T)

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
    aes(label =  paste(stat(eq.label),
                       stat(adj.rr.label),
                       sep = "~~~~")),
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
      color  = type
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
  mutate(var1.pred = (var1.pred/1.22)^(1/boxcox_l)) |>
  left_join(
    sed_data,
    by = join_by(id, year)
  ) |>
  select(year, obs=ssd_mean, var1.pred) |> 
  gather(type, ssy_box, -year) |> 
  ggplot(
    aes(
      x = year,
      y = ssy_box,
      color  = type
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
  select(year, obs=ssd_mean, var1.pred) |> 
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
