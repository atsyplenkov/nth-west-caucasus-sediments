library(tidyverse)
library(readxl)
library(hydroGOF)

# 1) Load Turowski data ---------------------------------------------------
turowski_all <-
  read_excel("data/tables/Turowski-etal-2010_table-5.xlsx",
    sheet = "All Data"
  )

# 2) Fit a hyperbolic model -----------------------------------------------
hyperbolic_model <-
  nls(
    mean_bedload_fraction ~ a / (b + mean_sus_load_kgsec),
    data = turowski_all,
    start = list(a = 1, b = 1)
  )

# Display the model summary
summary(hyperbolic_model)

# Plot the data and hyperbolic fit
plot(turowski_all$mean_sus_load_kgsec,
  turowski_all$mean_bedload_fraction,
  xlab = "mean_sus_load_kgsec",
  ylab = "mean_bedload_fraction",
  main = "Hyperbolic Fit", col = "blue"
)
lines(turowski_all$mean_sus_load_kgsec,
  predict(hyperbolic_model),
  col = "red", lwd = 2
)

# 3) Quality assessment ---------------------------------------------------
turowski_all$.predict <-
  predict(hyperbolic_model)

hydroGOF::gof(turowski_all$.predict, turowski_all$mean_bedload_fraction)

# 4) Save model -----------------------------------------------------------
library(qs)

# qs::qsave(
#   hyperbolic_model,
#   "workflow/02_rtop-interpolation/data/bedload_model.qs"
# )


# 5) Plot ----------------------------------------------------------------
library(tidyplots)

plot <- turowski_all |>
  tidyplot(x = mean_sus_load_kgsec, y = mean_bedload_fraction) |>
  add_data_points() +
  geom_line(aes(y = .predict), color = "#d55e00")

plot <- plot |>
  tidyplots::adjust_x_axis_title("SSD, kg/sec") |>
  tidyplots::adjust_y_axis_title("Fbed") |>
  adjust_size(width = 10, height = 10, unit = "cm")

save_plot(plot, "figures/figS2_bedload_model.png")
