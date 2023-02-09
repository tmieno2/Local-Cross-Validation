################################################################################
#^^^^^^^^^^^^^^^^^^^ EONR based selection functions^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################

#BRF

find_local_EONR_BRF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y, num.threads = 1, tune.parameters = "all")
  
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
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


#RF

find_local_EONR_RF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y , num.threads = 1, tune.parameters = "all")
  
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

# Linear

find_local_EONR_Linear <- function(data, test_data, train_data) {
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  
  
  linear <- lm(yield ~ theta_b2_2 * N_tgt + theta_b2_1 * N_tgt + theta_b1_2 * N_tgt + theta_b1_1 * N_tgt + Nk_2_1 * N_tgt + Nk_2_2 * N_tgt + Nk_1_1 * N_tgt + Nk_1_2 * N_tgt + plateau_2_1 * N_tgt + plateau_2_2 * N_tgt + plateau_1_1 * N_tgt + plateau_1_2 * N_tgt + I(N_tgt^2),
               data = train_data
  )
  
  
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(linear, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

#gam

find_local_EONR_gam <- function(data, test_data) {
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  
  gam <- gam(yield ~ s(N_tgt, k = 3), data = test_data)
  
  opt_EONR <-
    copy(N_data) %>%
    .[, y_hat := predict(gam, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ]]
  
  return(opt_EONR)
}

#CF

find_local_EONR_CF <- function(data, X_cf_train, Y, W_f, test_data) {
  
  eval(parse(text = paste0("X_cv = test_data[, .(", x_vars_exp, ")]"))) #* X for test data
  
  macf_tau <-
    grf::multi_arm_causal_forest(
      X_cf_train, Y, W_f,
      num.threads = 1
    )
  
  N_levels <- unique(test_data$N_tgt)
  
  
  macf_delta <-
    predict(
      macf_tau,
      newdata = X_cv
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := test_data[, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, c("N_high", "N_low") := tstrsplit(variable, " - ", fixed = TRUE)] %>%
    .[, N_dif := as.numeric(N_high) - as.numeric(N_low)]
  
  
  opt_EONR <-
    copy(macf_delta) %>%
    .[, profit := pCorn * value - pN * N_dif] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    #* if the max profit is negative
    .[profit < 0, N_high := N_levels[1]] %>%
    .[, .(aunit_id, N_high = as.numeric(N_high))] %>%
    setnames("N_high", "opt_N_hat")
  
  return(opt_EONR)
}


################################################################################
#^^^^^^^^^^^^^^^^^^^ yield based selection functions^^^^^^^^^^^^^^^^^^^^^^^^^^^#
################################################################################

yield_prediction_brf <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y, num.threads = 1, tune.parameters = "all")
  
  #add codes for yield prediction 
  
  yield_prediction <-
    copy(test_data) %>%
    .[, y_hat := predict(boosted_forest, newdata = .[, cov_list, with = FALSE])]%>%
    .[, diff_y_and_y_hat_squared := (yield - y_hat)^2]
  
  
  
  return(yield_prediction)
} 



yield_prediction_rf <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y, num.threads = 1, tune.parameters = "all")
  
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

################################################################################
#^^^^^^^^^^^^^^^Find true EONR by training on the entire dataset^^^^^^^^^^^^^^^^
################################################################################
find_local_EONR_BRF_train_on_the_entire_data <- function(entire_data, X_all, Y_all) {
  #--- do tuning ---#
  boosted_forest_all <- boosted_regression_forest(X_all, Y_all, num.threads = 1, tune.parameters = "all")
  
  N_data <- data.table(N_tgt = seq(min(entire_data$N_tgt), max(entire_data$N_tgt), by = 1))
  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(boosted_forest_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
} 


find_local_EONR_RF_train_on_the_entire_data <- function(entire_data, X_all, Y_all) {
  #--- do tuning ---#
  regression_forest_all <- regression_forest(X_all, Y_all,num.threads = 1, tune.parameters = "all" )
  
  N_data <- data.table(N_tgt = seq(min(entire_data$N_tgt), max(entire_data$N_tgt), by = 1))
  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

find_local_EONR_Linear_train_on_the_entire_data <- function(entire_data) {
  N_data <- data.table(N_tgt = seq(min(entire_data$N_tgt), max(entire_data$N_tgt), by = 1))
  
  
  linear_all <- lm(yield ~ theta_b2_2 * N_tgt + theta_b2_1 * N_tgt + theta_b1_2 * N_tgt + theta_b1_1 * N_tgt + Nk_2_1 * N_tgt + Nk_2_2 * N_tgt + Nk_1_1 * N_tgt + Nk_1_2 * N_tgt + plateau_2_1 * N_tgt + plateau_2_2 * N_tgt + plateau_1_1 * N_tgt + plateau_1_2 * N_tgt + I(N_tgt^2),
               data = entire_data
  )
  
  
  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(linear_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

# cf entire data

find_local_EONR_CF_train_on_the_entire_data <- function(data) {
  
  Y <- data[, yield] #* dependent variable
  W_f <- as.factor(data[, N_tgt]) #* treatment factor variable
  
  N_levels <- unique(data$N_tgt)
  
  
  eval(parse(text = paste0("X = data[, .(", x_vars_exp, ")]"))) #* X
  eval(parse(text = paste0("X_cv = data[, .(", x_vars_exp, ")]"))) #* X for test data
  
  macf_tau <-
    grf::multi_arm_causal_forest(
      X, Y, W_f, 
      num.threads = 1
    )
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Predict treatment effects
  # /*+++++++++++++++++++++++++++++++++++
  macf_delta <-
    predict(
      macf_tau,
      newdata = X_cv
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := data [, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, c("N_high", "N_low") := tstrsplit(variable, " - ", fixed = TRUE)] %>%
    .[, N_dif := as.numeric(N_high) - as.numeric(N_low)]
  
  macf_results <-
    copy(macf_delta) %>%
    .[, profit := pCorn * value - pN * N_dif] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    #* if the max profit is negative
    .[profit < 0, N_high := N_levels[1]] %>%
    .[, .(aunit_id, N_high = as.numeric(N_high))] %>%
    setnames("N_high", "opt_N_hat") 
  
  return(macf_results)
  
}


#*******************************************************************************
#*#<<<<<<<<<<<<<<<<<<<
#@@@@@@@@@@@@@@@@@@@@@ MSEs
#<<<<<<<<<<<<<<<<<<<<<
#*******************************************************************************



################################################################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^MSE of Local EONR^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################


Find_MSE_Local_EONR <- function(method1, method2, data) {
  mse <- data %>%
    filter(method %in% c(method1, method2)) %>%
    group_by(repeats, id) %>%
    summarise(mse = (diff(N_tgt))^2) %>%
    subset(., select = -c(id)) %>%
    group_by(repeats) %>%
    summarise(mse_each_repeat = sum(mse))
  
  final_mse <- mean(mse$mse_each_repeat)
}


################################################################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^ MSE of yield ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################

Find_MSE_of_yield <- function(method1, data) {
  mse <- data %>%
    filter(method %in% c(method1)) %>%
    group_by(repeats) %>%
    summarise(mse_sum_over_folds = sum(mse)) 
  
  final_mse <- mean(mse$mse_sum_over_folds)
  
} 


################################################################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^ MSE of true EONR ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################

Find_MSE_of_EONR_train_on_whole_data <- function(method1, data_trained_on_the_whole) {
  mse <- data_trained_on_the_whole %>%
    filter(method %in% c(method1)) %>%
    mutate(mse= (data$opt_N-N_tgt)^2)
  
  final_mse <- sum(mse$mse)  
  
} 
