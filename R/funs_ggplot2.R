library(ggplot2)
library(ggtext)
library(extrafont)

# Make labels use Barlow by default
ggplot2::update_geom_defaults("label", list(family = "Inter"))

# https://github.com/andrewheiss/cautioning-canary/blob/master/lib/graphics.R
theme_kbn <- 
  function(base_size = 11, base_family = "Inter") {
    
    ret <- 
      ggplot2::theme_bw(base_size, base_family) +
      ggplot2::theme(
        plot.title = element_text(size = rel(1.4), face = "bold",
                                              family = "Inter"),
        plot.subtitle = ggtext::element_markdown(size = rel(1), face = "plain",
                                                 family = "Inter"),
        plot.caption = ggtext::element_markdown(size = rel(0.8), color = "grey50", face = "plain",
                                                family = "Inter",
                                                margin = margin(t = 10)),
        panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
        panel.grid = element_line(linewidth = 0.30),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = rel(0.9), hjust = 0,
                                              family = "Inter", face = "bold"),
        strip.background = element_rect(fill = "grey85", colour = NA),
        axis.ticks = element_blank(),
        # axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.x = ggtext::element_markdown(margin = margin(t = 10)),
        # axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.y = ggtext::element_markdown(margin = margin(r = 10)),
        axis.text = element_text(family = "Inter", face = "plain"),
        axis.title = element_text(family = "Inter", face = "plain"),
        legend.key = element_blank(),
        legend.text = ggtext::element_markdown(size = rel(1), family = "Inter", face = "plain"),
        legend.box.margin = margin(t = -0.5, unit = "lines"),
        legend.margin = margin(t = 0),
        legend.position = "bottom",
        strip.text.x = element_text(
          margin = margin(t = 0.15, l = 0.15, b = 0.15, unit = "lines")
        ),
        panel.spacing = unit(5, "pt"),
        # panel.grid.major.x = element_blank(),
        legend.justification = "left",
        legend.key.height = unit(0.7, "lines"),
        legend.key.width = unit(0.7, "lines"),
        legend.spacing = unit(0.3, "lines"),
        legend.background = element_blank()
      )
    
    ret
  }
