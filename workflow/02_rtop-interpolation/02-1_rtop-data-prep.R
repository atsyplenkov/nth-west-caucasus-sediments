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
  # filter(status == "pristine") |>
  filter(is.na(status2)) |>
  # filter(id != "83216") |>
  # filter(id != "83395") |>
  # filter(id != "83333") |>
  # filter(id != "83368") |> 
  # filter(id != "83314") |> 
  # filter(id != "83207") |> 
  # filter(id != "83214") |>
  # filter(id != "83395") |>
  # filter(id != "83340") |> 
  # filter(id != "83216") |> 
  # filter(id != "83333") |> 
  # filter(id != "83437") |> 
  pull(id)

north_ids <- 
  kbn_gages |> 
  pull(id)

# 2) Watersheds -----------------------------------------------------------
ws_all <- 
  st_read(
    here("data/vector/kbn_ws_30dec/kbn_ws_30dec.shp"),
    query = "SELECT id, area FROM kbn_ws_30dec"
  ) |> 
  mutate(
    id = as.character(id)
  ) |> 
  filter(id %in% north_ids) |>
  ms_simplify(keep = 0.3)

ws_new <- 
  st_read(
    here("data/vector/kbn_ws-predict/kbn_ws-predict.shp"),
    query = "SELECT * FROM \"kbn_ws-predict\" WHERE id IS NULL"
  ) |> 
  transmute(id = river,
            area = a) |> 
  rename(geometry = "_ogr_geometry_")

# Observed data
ws_obs <- 
  ws_all |> 
  filter(id %in% rtop_ids)

# Predict to
ws_pred <- 
  ws_all |> 
  bind_rows(ws_new)

# 3) Sediment data --------------------------------------------------------
sed_data <- 
  qs::qread(
    here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs")
  ) |> 
  mutate(id = as.character(id)) |> 
  select(id:ssd_mean)

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

set.seed(123)
with_cv <- 
  ws_obs_box |> 
  kbn_rtop(.year = 1977, .iter = 2000, .boxcox = boxcox_l)

with_cv$.cv_res

with_cv$.cv_df |> 
  pluck(1) |> 
  ggplot(
    aes(
      x = obs^boxcox_l,
      y = var1.pred^boxcox_l
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
  ggrepel::geom_text_repel(
    aes(label = id),
    size = 3
  ) + 
  tune::coord_obs_pred()

# set.seed(123)
# no_cv <- 
#   ws_obs_box |> 
#   kbn_rtop(.iter = 2000, .cv = F)
# 
# cv_test <- 
#   no_cv |> 
#   filter(id %in% with_cv$id) |> 
#   select(id, contains("var1")) |>
#   magrittr::set_names(c('id', 'cv_pred', "cv_var")) |> 
#   left_join(with_cv, by = join_by(id))
# 
# cv_test |> 
#   ggplot() +
#   geom_point(
#     aes(x = obs, y = cv_pred, color = "CV")
#   ) +
#   geom_point(
#     aes(x = obs, y = var1.pred, color = "NO CV")
#   )
# 

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

# plan(sequential)

# CV metrics --------------------------------------------------------------
ssd_uk |> 
  unnest(c(.cv_res)) |> 
  ggdist::mean_qi(CCC)

ssd_uk |>
  select(.cv_df) |> 
  unnest(c(.cv_df)) |> 
  # filter(id != "83216") |> 
  yardstick::ccc(truth = obs, estimate = var1.pred)


# Find rtop outliers based on Cook's Distance

cv_df <- 
  ssd_uk |>
  select(.cv_df) |> 
  unnest(c(.cv_df))

mod <-  
  lm(var1.pred ~ obs, data = cv_df)

cooksd <- 
  cooks.distance(mod)

cutoff <- 4 / length(cooksd)
# cutoff <- 2 * mean(cooksd)

cutoff_df <- 
  cv_df[cooksd > cutoff, ] |>
  bind_rows(
    filter(
      cv_df,
      id == "83340" & year == 2012 |
        id == "83216" & year == 1966 |
        id == "83395" & year == 2016 |
        id == "83437" & year == 2015 |
        id == "83207" & year == 2011 |
        id == "83395" & year == 2019 |
        id == "83424" & year == 1957 |
        id == "83437" & year == 1969 |
        id == "83385" & year == 1980 |
        id == "83262" & year == 2006
    )
  )

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
                       stat(adj.rr.label),
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

plotly::ggplotly(cv1_plot)

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

ssd_uk2 |>
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
                       stat(adj.rr.label),
                       sep = "~~~~")),
    formula = y ~ x 
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x
  ) +
  ggrepel::geom_text_repel(
    aes(label = id),
    size = 3
  ) + 
  tune::coord_obs_pred()




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
