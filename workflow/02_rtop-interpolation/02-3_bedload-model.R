library(tidyverse)
library(readxl)
library(ggpmisc)

# 1) Load Turowski data ---------------------------------------------------
turowski_all <- 
  read_excel("data/tables/Turowski-etal-2010_table-5.xlsx",
             sheet = "All Data")

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
plot(data$mean_sus_load_kgsec, data$mean_bedload_fraction, 
     xlab = "mean_sus_load_kgsec", ylab = "mean_bedload_fraction", 
     main = "Hyperbolic Fit", col = "blue")
lines(data$mean_sus_load_kgsec, predict(hyperbolic_model), col = "red", lwd = 2)

# 3) Quality assessment ---------------------------------------------------
turowski_all$.predict <- 
  predict(hyperbolic_model)

hydroGOF::gof(data$.predict, data$mean_bedload_fraction)
