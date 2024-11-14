# 1) Pretty print range of volume -----------------------------------------
sed_to_vol <-
  function(
      .ss,
      .low_dens = 1.1,
      .high_dens = 1.5) {
    paste0(
      atslib::smart_round(.ss / .high_dens),
      "-",
      atslib::smart_round(.ss / .low_dens)
    )
  }

# 2) Yardstick NSE --------------------------------------------------------
library(tidymodels) # CRAN v1.1.1
library(rlang) # CRAN v1.1.1

# Vector implementation
nse_impl <-
  function(
      truth,
      estimate,
      case_weights = NULL) {
    1 - (sum((truth - estimate)^2) / sum((truth - mean(truth))^2))
  }

nse_vec <-
  function(
      truth,
      estimate,
      na_rm = TRUE,
      case_weights = NULL,
      ...) {
    check_numeric_metric(truth, estimate, case_weights)

    if (na_rm) {
      result <-
        yardstick_remove_missing(truth, estimate, case_weights)

      truth <- result$truth
      estimate <- result$estimate
      case_weights <- result$case_weights
    } else if (yardstick_any_missing(truth, estimate, case_weights)) {
      return(NA_real_)
    }

    nse_impl(truth, estimate, case_weights = case_weights)
  }


# Dataframe implementation
nse <- function(data, ...) {
  UseMethod("nse")
}

nse <-
  new_numeric_metric(nse, direction = "maximize")

nse.data.frame <-
  function(
      data,
      truth,
      estimate,
      na_rm = TRUE,
      case_weights = NULL,
      ...) {
    numeric_metric_summarizer(
      name = "nse",
      fn = nse_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm,
      case_weights = !!enquo(case_weights)
    )
  }
