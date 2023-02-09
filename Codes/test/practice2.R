yield_prediction_brf <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y)
  
  #add codes for yield prediction 
  
  yield_prediction <-
    copy(test_data) %>%
    .[, y_hat := predict(boosted_forest, newdata = .[, cov_list, with = FALSE])]%>%
    .[, diff_y_and_y_hat_squared := (yield - y_hat)^2]
  
  
  
  return(yield_prediction)
} 



yield_prediction_rf <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y)
  
  yield_prediction <-
    copy(test_data) %>%
    .[, y_hat := predict(regression_forest, newdata = .[, cov_list, with = FALSE])]%>%
    .[, diff_y_and_y_hat_squared := (yield - y_hat)^2]
  
  
  return(yield_prediction)
}



yield_prediction_linear <- function(data, test_data, train_data) {
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  
  
  linear <- lm(yield ~ theta_b2_2 * N_tgt + theta_b2_1 * N_tgt + theta_b1_2 * N_tgt + theta_b1_1 * N_tgt + Nk_2_1 * N_tgt + Nk_2_2 * N_tgt + Nk_1_1 * N_tgt + Nk_1_2 * N_tgt + plateau_2_1 * N_tgt + plateau_2_2 * N_tgt + plateau_1_1 * N_tgt + plateau_1_2 * N_tgt + I(N_tgt^2),
               data = train_data
  )
  
  
  yield_prediction <-
    copy(test_data) %>%
    .[, y_hat := predict( linear, newdata = .[, cov_list, with = FALSE])]%>%
    .[, diff_y_and_y_hat_squared := (yield - y_hat)^2]
  
  
  return(yield_prediction)
}










find_yield_based_mse <- function(n, spatial_folds, data) {
  temp_fold <- spatial_folds[n, ]
  
  train_data <- temp_fold$training_data[[1]]
  test_data <- temp_fold$test_data[[1]]
  
  X <- as.matrix(train_data[, cov_list, with = FALSE])
  Y <- train_data$yield
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  mse_brf <- yield_prediction_brf(data, X, Y, test_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (BRF)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## RF
  # /*+++++++++++++++++++++++++++++++++++
  mse_rf <- yield_prediction_rf(data, X, Y, test_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (RF)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Linear
  # /*+++++++++++++++++++++++++++++++++++
  mse_linear <- yield_prediction_linear(data, test_data, train_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (Linear)"]
  
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  mse_eonr_data <-
    rbind(mse_brf, mse_rf, mse_linear) %>%
    .[, id := temp_fold$id] %>%
    .[, repeats := temp_fold$repeats]
  
  return(mse_eonr_data)


}


test <- lapply(1:60, function(x) find_yield_based_mse(x, spatial_folds, data)) %>%
  rbindlist()

Find_MSE_of_yield <- function(method, data) {
  mse <- data %>%
    filter(method %in% method) %>%
    group_by(repeats) %>%
    summarise(mse_sum_over_folds = sum(mse)) 
  
  final_mse <- mean(mse$mse_sum_over_folds)
}

mse_test <- 
  
  
  
  Find_MSE_of_yield <- function(method1, data) {
    mse <- data %>%
      filter(method %in% c(method1)) %>%
      group_by(repeats) %>%
      summarise(mse_sum_over_folds = sum(mse)) 
    
    final_mse <- mean(mse$mse_sum_over_folds)
   
  } 
  
  
mean(mse$mse_sum_over_folds)  
  
  
mse_of_yield_brf <- Find_MSE_of_yield("mse (BRF)", test)  #4705139965

mse_of_yield_rf <- Find_MSE_of_yield("mse (RF)", test)

mse_of_yield_Linear <- Find_MSE_of_yield("mse (Linear)", test)


  

