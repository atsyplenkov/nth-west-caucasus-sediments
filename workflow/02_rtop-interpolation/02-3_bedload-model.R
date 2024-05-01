library(tidyverse)
library(readxl)
library(hydroGOF)

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
plot(turowski_all$mean_sus_load_kgsec, 
     turowski_all$mean_bedload_fraction, 
     xlab = "mean_sus_load_kgsec", 
     ylab = "mean_bedload_fraction", 
     main = "Hyperbolic Fit", col = "blue")
lines(turowski_all$mean_sus_load_kgsec, 
      predict(hyperbolic_model), col = "red", lwd = 2)

# 3) Quality assessment ---------------------------------------------------
turowski_all$.predict <- 
  predict(hyperbolic_model)

hydroGOF::gof(turowski_all$.predict, turowski_all$mean_bedload_fraction)

# 4) Save model -----------------------------------------------------------
library(qs)

qs::qsave(
  hyperbolic_model,
  "workflow/02_rtop-interpolation/data/bedload_model.qs"
)
