library(ggplot2)
library(ggtext)
library(showtext)
font_add_google("Merriweather", "Merriweather")
showtext_auto()
showtext::showtext_opts(dpi = 500)

# Make labels use Barlow by default
ggplot2::update_geom_defaults(
  "label",
  list(family = "Merriweather")
)

# https://github.com/andrewheiss/cautioning-canary/blob/master/lib/graphics.R
theme_kbn <-
  function(base_size = 11, base_family = "Merriweather") {
    ret <-
      ggplot2::theme_bw(base_size, base_family) +
      ggplot2::theme(
        plot.title = element_text(
          size = rel(1.4),
          face = "bold",
          family = "Merriweather"
        ),
        plot.subtitle = ggtext::element_markdown(
          size = rel(1),
          face = "plain",
          family = "Merriweather"
        ),
        plot.caption = ggtext::element_markdown(
          size = rel(0.8), color = "grey50", face = "plain",
          family = "Merriweather",
          margin = margin(t = 10)
        ),
        panel.border = element_rect(
          color = "grey50",
          fill = NA, linewidth = 0.15
        ),
        panel.grid = element_line(linewidth = 0.30),
        panel.grid.minor = element_blank(),
        strip.text = element_text(
          size = rel(0.9), hjust = 0,
          family = "Merriweather", face = "bold"
        ),
        strip.background = element_rect(fill = "grey85", colour = NA),
        axis.ticks = element_blank(),
        axis.title.x = ggtext::element_markdown(
          size = rel(0.9),
          family = "Merriweather",
        ),
        axis.title.y = ggtext::element_markdown(
          size = rel(0.9),
          family = "Merriweather",
        ),
        # axis.title.x = ggtext::element_markdown(size = rel(0.9)),
        # axis.title.y = element_text(margin = margin(r = 10)),
        # axis.title.y = ggtext::element_markdown(size = rel(0.9)),
        axis.text = element_text(family = "Merriweather", face = "plain"),
        axis.title = element_text(family = "Merriweather", face = "plain"),
        legend.key = element_blank(),
        legend.text = ggtext::element_markdown(
          size = rel(1),
          family = "Merriweather", face = "plain"
        ),
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
        legend.background = element_blank(),
        plot.margin = margin(t = 5.5, r = 12, b = 5.5, l = 5.5)
      )

    ret
  }

kbn_colors <-
  function(
      n = 4,
      begin = 0.05,
      end = 0.65,
      view = FALSE) {
    inferno_pal <-
      viridisLite::rocket(n = n, begin = begin, end = end)

    if (view) {
      monochromeR::view_palette(inferno_pal)
    } else {
      return(inferno_pal)
    }
  }

# Save function ----------------------------------------------------------
mw_save <-
  function(filename,
           plot,
           dpi = 1000,
           w = 16,
           h = 12,
           units = "cm") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = ragg::agg_png,
      dpi = dpi,
      width = w,
      height = h,
      units = units
    )
  }