# libraries ---------------------------------------------------------------
library(tidyverse)
library(qs)
library(here)

source(here("R", "funs_ggplot2.R"))

# ggplot2 setup -----------------------------------------------------------
theme_set(theme_kbn())

my_pal <- kbn_colors(n = 2)

# load data ---------------------------------------------------------------
ssd <- 
  qread("R/02_Features/data/db_ssd-hyde-tc.qs")

gmba <- 
  qread("R/02_Features/data/db_gmda-dem.qs")

ssd_ <- 
  ssd %>% 
  left_join(gmba, by = "id") |> 
  mutate(period = ifelse(year < 1992, "XX", "XXI"))

# Subset data -------------------------------------------------------------
# Mean annual SSD
ssd_mean_av <- 
  ssd_ |> 
  select(id:ssd_mean) |> 
  group_by(id) |> 
  mutate(ssd_a = data.table::rleid(is.na(ssd_mean))) |> 
  filter(!is.na(ssd_mean)) |> 
  group_by(id, ssd_a)  |>  
  summarise(start = first(year),
            end = last(year),
            n = n(),
            .groups = "drop") %>% 
  mutate(type = "SSD[mean]",
         .before = 1)

# Max annual SSD
ssd_max_av <- 
  ssd_ |> 
  select(id:year, ssd_max) |> 
  group_by(id) |> 
  mutate(ssd_a = data.table::rleid(is.na(ssd_max))) |> 
  filter(!is.na(ssd_max)) |> 
  group_by(id, ssd_a)  |>  
  summarise(start = first(year),
            end = last(year),
            n = n(),
            .groups = "drop") %>% 
  mutate(type = "SSD[max]",
         .before = 1)

# Bind together
ssd_viz <- 
  bind_rows(
    ssd_mean_av, ssd_max_av
  ) |> 
  group_by(id, type) |> 
  mutate(n = sum(n)) |> 
  ungroup()

id_order <- 
  ssd_viz |> 
  filter(type == "SSD[mean]") |> 
  distinct(id, n) |> 
  arrange(desc(n))

# Plot --------------------------------------------------------------------
viz_miss <- 
  ssd_mean_av |> 
  mutate(id = factor(id, levels = id_order$id, ordered = T)) |> 
  ggplot() +
  geom_segment(
    aes(x = start, xend = end,
        y = id, yend = id,
        color = type),
    linewidth = 1.1,
    position = position_nudge(y  = 0.2)) +
  geom_segment(
    data = ssd_max_av |> 
      mutate(id = factor(id, levels = id_order$id, ordered = T)),
    aes(x = start, xend = end,
        y = id, yend = id,
        color = type),
    linewidth = 1.1,
    position = position_nudge(y  = -0.2)) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(
    # limits = c(-24, 5),
    breaks = scales::breaks_pretty(10)
  ) +
  scale_color_manual(
    values = my_pal,
    labels = scales::parse_format()
  ) +
  labs(
    y = "Gauging station ID",
    x = "",
    color = NULL
  ) +
  theme(panel.grid.major.y = element_line(color = "grey90"))

# Save --------------------------------------------------------------------
ggsave(
  viz_miss, 
  filename = "R/01_Sediment-database/figures/data-availability.png",
  device = ragg::agg_png, 
  dpi = 1000,
  width = 130, 
  height = 164, 
  units = "mm"
)




