# The graphical abstract was created in Inkscape
# Some figures produced with ggplot was produced
# here and further inserted in Inkscape

library(here)
library(tidyverse)

source(here("R", "funs_ggplot2.R"))

clrs <- MetBrewer::met.brewer("Johnson", n = 8)
clrs[3] <- "grey10"

sed_den <- readxl::read_excel(
  "data/tables/krasnodar_res_vol.xlsx",
  range = "A1:N7"
) |>
  filter(!is.na(time2)) |>
  transmute(
    time1,
    time2,
    Period = paste0(time1, "-", time2),
    E = `denudation rate, mm/yr`,
    S = `sedimentation rate, %`
  ) |>
  mutate(across(c(time1, time2), ~as.numeric(.x))) |>
  mutate(Period = as_factor(Period))

plot_sed <- sed_den |>
  ggplot() +
  geom_line(
    aes(
      x = Period,
      y = E,
      color = "Denudation",
      group = NA
    ),
    lwd = 1.2
  ) +
  scale_x_discrete(
    name = "",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "**Denudation rate _E_ [mm/yr]**",
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_color_manual(
    name = "",
    values = clrs[c(7, 2)],
    breaks = c("Sedimentation", "Denudation"),
    guide = guide_legend(
      nrow = 2
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.15),
    legend.text = ggtext::element_markdown(
      family = "Merriweather",
      size = 9
    ),
    axis.ticks.x = element_line(color = "grey80"),
    panel.grid.major = element_blank(),
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.background = element_blank(),
    plot.background = element_blank()
  )

plot_sed

tibble(
  year = seq(1973, 2021),
  y = rnorm(length(seq(1973, 2021)))
) |>
  ggplot(
    aes(x = year, y = y)
  ) +
  scale_x_continuous(
    breaks = c(1973, 1975, 1985, 1991, 1993, 2005, 2016, 2021),
    limits = c(1973, 2021)
  )
