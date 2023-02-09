MSE_local_EONR_criterion <- function(All_EONR, method) {
  
 filter_method <- filter(All_EONR, method==c("S-learner (BRF)", "S-learner (GAM)"))

 obtain_mse <- filter_method %>% group_by(repeats)%>%
   mutate(mse= (N_tgt-lag(N_tgt, default = first(N_tgt)))^2)
   
 mse <- obtain_mse %>% subset(., select=-c(N_tgt, method, id)) %>% group_by(repeats) %>% summarise(mse_final=sum(mse))
 
 mse <- 
   
   
 mutate(diff_row = mpg - lag(mpg, default = first(mpg)))
 mutate(diff_row = mpg - lag(mpg))
 
 
  
 diff_df <- df[-1,] - df[-nrow(df),]
  
  
}



agg_tbl <- df %>% group_by(department) %>% 
  summarise(sum_salary = sum(salary),
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()
df2











find_local_EONR_BRF <- function(data_m, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y)
  
  N_data <- data.table(N_tgt = seq(min(data_m$N_tgt), max(data_m$N_tgt), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(boosted_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}





find_local_EONR_all <- function(i, spatial_folds, data_m) {
  temp_fold <- spatial_folds[i, ]
  
  train_data <- temp_fold$training_data[[1]]
  test_data <- temp_fold$test_data[[1]]
  
  X <- as.matrix(train_data[, cov_list, with = FALSE])
  Y <- train_data$yield
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_BRF <-
    find_local_EONR_BRF(data_m, X, Y, test_data) %>%
    .[, .(mean_N_tgt = mean(N_tgt))] %>%
    .[, method := "S-learner (BRF)"]
  
  
  return(opt_EONR_BRF)
}


test <- lapply(1:nrow(spatial_folds), function(x) find_local_EONR_all(x, spatial_folds, data_m)) %>%
  rbindlist()
