kbn_rtop <- 
  function(
    .df, 
    .year = 1977, 
    .iter = 1000, 
    .boxcox = 0.15,
    .cooks_cutoff = FALSE
  ){
    
    if (.cooks_cutoff) {
      
      cutoff_id <- 
        cutoff_df |> 
        filter(year == .year) |> 
        pull(id)
      
      ws_obs_sy <- 
        .df |> 
        filter(year == .year) |> 
        filter(!id %in% cutoff_id)
      
    } else {
      
      ws_obs_sy <- 
        .df |> 
        filter(year == .year)
      
    }
    
    # Kriging
    rtopObj <- 
      createRtopObject(ws_obs_sy, ws_pred,
                       formulaString = obs ~ 1,
                       params = params)
    
    rtopObj  <- 
      rtopVariogram(rtopObj)
    
    rtopObj <-
      rtopFitVariogram(rtopObj, maxn = .iter)
    
    # Cross-Validation
    rtopObj.cv <- 
      rtopKrige(rtopObj, cv = TRUE)
    
    rtop_cv_pred <- 
      rtopObj.cv$predictions$var1.pred #^(1/.boxcox)
    
    rtop_cv_obs <- 
      rtopObj.cv$predictions$obs #^(1/.boxcox)
    
    rtop_cv_res <- 
      rtop_cv_pred - rtop_cv_obs
    
    cv_res <- 
      tibble(
        bias = mean(rtop_cv_res),
        pBIAS = hydroGOF::pbias(rtop_cv_pred, rtop_cv_obs),
        RMSE = hydroGOF::rmse(rtop_cv_pred, rtop_cv_obs),
        NSE = hydroGOF::NSE(rtop_cv_pred, rtop_cv_obs),
        R2 = yardstick::rsq_vec(rtop_cv_obs, rtop_cv_pred),
        CCC = yardstick::ccc_vec(rtop_cv_obs, rtop_cv_pred)
      )
    
    cv_df <- 
      rtopObj.cv$predictions |> 
      st_drop_geometry() |> 
      mutate(year = .year)
    
    # Predictions
    rtopObj.pred  <- 
      rtopKrige(rtopObj.cv)
    
    tibble(
      year = .year,
      .cv_res = list(
        cv_res
      ),
      .cv_df = list(
        cv_df
      ),
      .pred_df = list(
        rtopObj.pred$predictions |> 
          st_drop_geometry() |> 
          mutate(year = .year)
      )
    )
    
    
    # rtopObj$predictions |> 
    #   st_drop_geometry() |> 
    #   select(id, var1.pred, var1.var, sumWeights) |> 
    #   mutate(year = .year)
    
  }
# 
# 
# 
# kbn_rtop <- 
#   function(
#     .df, 
#     .year = 1977, 
#     .iter = 1000, 
#     .boxcox = 0.15,
#     .cooks_cutoff = FALSE
#     ){
#     
#     if (.cooks_cutoff) {
#       
#       cutoff_id <- 
#         cutoff_df |> 
#         filter(year == .year) |> 
#         pull(id)
#       
#       ws_obs_sy <- 
#         .df |> 
#         filter(year == .year) |> 
#         filter(!id %in% cutoff_id)
#       
#     } else {
#       
#       ws_obs_sy <- 
#         .df |> 
#         filter(year == .year)
#       
#     }
#     
#     # Kriging
#     rtopObj <- 
#       createRtopObject(ws_obs_sy, ws_pred,
#                        formulaString = obs ~ 1,
#                        params = params)
#     
#     rtopObj  <- 
#       rtopVariogram(rtopObj)
#     
#     rtopObj <-
#       rtopFitVariogram(rtopObj, maxn = .iter)
#     
#     # Cross-Validation
#     rtopObj.cv <- 
#       rtopKrige(rtopObj, cv = TRUE)
#     
#     rtop_cv_pred <- 
#       rtopObj.cv$predictions$var1.pred #^(1/.boxcox)
#     
#     rtop_cv_obs <- 
#       rtopObj.cv$predictions$obs #^(1/.boxcox)
#     
#     rtop_cv_res <- 
#       rtop_cv_pred - rtop_cv_obs
#     
#     cv_res <- 
#       tibble(
#         bias = mean(rtop_cv_res),
#         pBIAS = hydroGOF::pbias(rtop_cv_pred, rtop_cv_obs),
#         RMSE = hydroGOF::rmse(rtop_cv_pred, rtop_cv_obs),
#         NSE = hydroGOF::NSE(rtop_cv_pred, rtop_cv_obs),
#         R2 = yardstick::rsq_vec(rtop_cv_obs, rtop_cv_pred),
#         CCC = yardstick::ccc_vec(rtop_cv_obs, rtop_cv_pred)
#       )
#     
#     # Predictions
#     rtopObj  <- 
#       rtopKrige(rtopObj)
#     
#     tibble(
#       year = .year,
#       .cv_res = list(
#         cv_res
#       ),
#       .cv_df = list(
#         rtopObj.cv$predictions |> 
#           st_drop_geometry() |> 
#           mutate(year = .year)
#       ),
#       .pred_df = list(
#         rtopObj$predictions |> 
#           st_drop_geometry() |> 
#           mutate(year = .year)
#       )
#     )
# 
#     
#     # rtopObj$predictions |> 
#     #   st_drop_geometry() |> 
#     #   select(id, var1.pred, var1.var, sumWeights) |> 
#     #   mutate(year = .year)
#     
#   }
