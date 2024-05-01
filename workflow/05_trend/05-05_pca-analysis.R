library(tidyverse)
library(imputeTS)
library(factoextra)
library(lubridate)

# Load data ---------------------------------------------------------------
sed_df <- qs::qread("workflow/05_trend/data/ssd_both.qs")
landcover_df <- qs::qread("workflow/05_trend/data/landcover_buchner2020.qs")
worldclim_df <- qs::qread("workflow/05_trend/data/worldclim.qs")
glims_df <- qs::qread("workflow/05_trend/data/glims.qs")

# Landcover Chars ---------------------------------------------------------
lc_desc <-
  list(
    aggregate(landcover_df, CroplandArea ~ river, "mean"),
    aggregate(landcover_df, ForestArea ~ river, "mean"),
    aggregate(glims_df, GlimsArea ~ river, "mean")
  ) |>
  reduce(left_join, by = join_by(river)) |>
  left_join(sed_df |>
              select(river, Area) |>
              distinct(),
            by = join_by(river)) |>
  mutate(GlimsArea = GlimsArea / 10 ^ 6) |>
  mutate(across(contains("Area"), ~ {
    .x / Area
  })) |> 
  mutate(across(where(is.numeric), ~round(.x, 3)))

library(santoku)

lc_desc |> 
  transmute(
    river,
    Crop = chop(CroplandArea, c(0, 0.2), lbl_dash()),
    Forest = chop(ForestArea, c(0, 0.5), lbl_dash()),
    Glacier = chop(GlimsArea, c(0, 0.001), lbl_dash())
  )


# Merge together ----------------------------------------------------------
landcover_complete <- 
  landcover_df |> 
  group_by(river) |> 
  complete(Year = seq(1960, 2021, by = 1)) |> 
  mutate(across(c(CroplandArea, ForestArea), ~na_interpolation(.x))) |> 
  ungroup()

worldclim_complete <- 
  worldclim_df |> 
  pluck(2) |> 
  mutate(Year = year(date)) |> 
  group_by(river, Year) |> 
  reframe(
    prec_cv = mean(prec_cv),
    prec = sum(prec),
    tmax = mean(tmax),
    tmin = mean(tmin)
  )

sed_complete <- 
  sed_df |> 
  mutate(SYTotal = SDTotal * 31536 / Area) |> 
  # filter(SSDFlag == "Obs") |>
  select(river, Year, SYTotal, Area) |> 
  filter(between(Year, 1960, 2021)) 

df_complete <- 
  list(
    sed_complete,
    landcover_complete,
    worldclim_complete
  ) |> 
  reduce(left_join, by = join_by(river, Year))

# Marginal effects --------------------------------------------------------
library(tidymodels)

model_sq  <- 
  lm(SYTotal ~ prec_med + I(prec_med^2) + prec_med*river, data = df_complete)
broom::glance(model_sq)
tidy(model_sq)

# Extract the two civil_liberties coefficients
civ_lib1 <- tidy(model_sq) |> 
  filter(term == "prec_med") |> 
  pull(estimate)
civ_lib2 <- tidy(model_sq) |> 
  filter(term == "I(prec_med^2)") |> 
  pull(estimate)

# Make a little function to do the math
civ_lib_slope <- function(x) civ_lib1 + (2 * civ_lib2 * x)

# Point-slope formula: (y - y1) = m(x - x1)
find_intercept <- function(x1, y1, slope) {
  intercept <- slope * (-x1) + y1
  return(intercept)
}

library(ggtext)
clrs <- MetBrewer::met.brewer("Johnson")

# Format numbers in pretty ways
nice_number <- label_number(style_negative = "minus", accuracy = 0.01)
nice_p <- label_pvalue(prefix = c("p < ", "p = ", "p > "))

tangents <- 
  model_sq |> 
  augment(newdata = tibble(prec_med = c(800, 1200, 1400))) |> 
  mutate(slope = civ_lib_slope(prec_med),
         intercept = find_intercept(prec_med, .fitted, slope)) |> 
  mutate(nice_label = glue::glue("P: {prec_med}<br>",
                           "Fitted SSY: {nice_number(.fitted)}<br>",
                           "Slope: **{nice_number(slope)}**"))

ggplot(df_complete,
       aes(y = SYTotal,
           x = prec_med)) +
  geom_point(color = "grey30") +
  stat_smooth(
    method = "lm",
    formula = y ~ x + I(x ^ 2),
    linewidth = 1,
    color = clrs[4]
  ) +
  geom_abline(
    data = tangents,
    aes(slope = slope,
        intercept = intercept),
    linewidth = 0.5,
    color = clrs[2],
    linetype = "21"
  ) +
  geom_point(
    data = tangents,
    aes(x = prec_med, y = .fitted),
    size = 4,
    shape = 18,
    color = clrs[2]
  ) +
  geom_richtext(data = tangents,
                aes(x = prec_med, y = .fitted, label = nice_label),
                vjust = 0)

# Logit model -------------------------------------------------------------
library(emmeans)
library(sjPlot)

df_model <- 
  df_complete |> 
  # select(river, Year, SYTotal, prec) |> 
  # left_join(lc_desc, by = join_by(river)) |> 
  mutate(across(contains("Area"), ~{.x / Area})) |>
  mutate(
    LC = ifelse(river == "Pshish", "P", "KL")
  ) |> 
  filter(Year > 1987)

m <- 
  lm(SYTotal ~ prec + prec * CroplandArea, data = df_model)
broom::glance(m)
tidy(m)

m |> 
  plot_model(type = "pred", terms = c("prec", "CroplandArea", "ForestArea")) +
  coord_cartesian(ylim = c(0, NA), expand = F)

emmeans(m, pairwise ~ prec | CroplandArea,
        cov.reduce = range)


emmeans(m, ~ prec | LC, cov.reduce = range,
        at = list(prec_med = seq(600, 1500, 50)))

em <- 
  emmeans(m, pairwise ~ prec_med | river,
         at = list(prec_med = seq(700, 1500, 50)))$emmeans

as_tibble(em) |> 
  ggplot(
    aes(
      x = prec_med,
      y = emmean,
      color = river
    )
  ) +
  geom_line()

# All dat -----------------------------------------------------------------
df_crop |> 
  ggplot() +
  geom_point(
    aes(
      y = SYTotal,
      x = prec_med,
      color = river
    )
  ) +
  geom_line(
    data = augment(m),
    aes(
      x = prec_med,
      y = .fitted
    )
  )



# PCA ---------------------------------------------------------------------
mod <- prcomp(df_complete[, -1:-2], scale = T)

factoextra::fviz_pca_biplot(mod, addEllipses = T, habillage = )

fviz_pca_biplot(
  mod, 
  alpha.ind = 1,
  habillage = df_complete$river,
  repel = TRUE,
  label = "var"
) +
  # ADD ggforce's ellipses
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups)) +
  theme(legend.position = 'bottom') +
  coord_equal()
