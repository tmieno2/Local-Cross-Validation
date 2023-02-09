
################################################################################
# ^^^^^^^^^^^^^^^Find true EONR by training on the entire dataset^^^^^^^^^^^^^^^^
################################################################################
find_local_EONR_BRF_train_on_the_entire_data <- function(entire_data, X_all, Y_all) {
  #--- do tuning ---#
  boosted_forest_all <- boosted_regression_forest(X_all, Y_all, tune.parameters = "all")

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
  regression_forest_all <- regression_forest(X_all, Y_all, tune.parameters = "all")

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



#*******************************************************************************
#* #<<<<<<<<<<<<<<<<<<<
# @@@@@@@@@@@@@@@@@@@@@ MSEs
#<<<<<<<<<<<<<<<<<<<<<
#*******************************************************************************



################################################################################
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^MSE of Local EONR^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^ MSE of yield ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################

Find_MSE_of_yield <- function(method1, data) {
  mse <- data %>%
    filter(method %in% c(method1)) %>%
    group_by(repeats) %>%
    summarise(mse_sum_over_folds = sum(mse))

  final_mse <- mean(mse$mse_sum_over_folds)
}


################################################################################
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^ MSE of true EONR ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################

Find_MSE_of_EONR_train_on_whole_data <- function(method1, data_trained_on_the_whole) {
  mse <- data_trained_on_the_whole %>%
    filter(method %in% c(method1)) %>%
    mutate(mse = (data$opt_N - N_tgt)^2)

  final_mse <- sum(mse$mse)
}
