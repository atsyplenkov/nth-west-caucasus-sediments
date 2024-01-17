# 1) Pretty print range of volume -----------------------------------------
sed_to_vol <- 
  function(
    .ss,
    .low_dens = 1.1,
    .high_dens = 1.5
  ){
    
    paste0(
      atslib::smart_round(.ss / .high_dens),
      "-",
      atslib::smart_round(.ss / .low_dens)
    ) 
    
  }

