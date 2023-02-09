find_vra_whole_all <- function(data) {
  X_all <- as.matrix(data[, cov_list, with = FALSE])
  Y_all <- data$yield

  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  opt_vra_BRF_whole_field <- find_opt_vra_BRF_whole_field(data, X_all, Y_all)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## RF
  # /*+++++++++++++++++++++++++++++++++++
  opt_vra_RF_whole_field <- find_opt_vra_RF_whole_field(data, X_all, Y_all)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Linear
  # /*+++++++++++++++++++++++++++++++++++
  opt_vra_Linear_whole_field <- find_opt_vra_Linear_whole_field(data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## CF
  # /*+++++++++++++++++++++++++++++++++++
  opt_vra_CF_whole_field <- find_opt_vra_CF_whole_field(data)


  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  eonr_data <-
    rbind(
      opt_vra_BRF_whole_field,
      opt_vra_RF_whole_field,
      opt_vra_Linear_whole_field,
      opt_vra_CF_whole_field
    )

  return(eonr_data)
}

################################################################################
# ^^^^^^^^^^^^^^^Find true EONR by training on the entire dataset^^^^^^^^^^^^^^^^
################################################################################
find_opt_vra_BRF_whole_field <- function(entire_data, X_all, Y_all) {
  #--- do tuning ---#
  boosted_forest_all <-
    boosted_regression_forest(
      X_all,
      Y_all,
      num.threads = 1,
      tune.parameters = "all",
      num.trees = 1000
    )

  N_data <- data.table(N = seq(min(entire_data$N), max(entire_data$N), by = 1))

  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(boosted_forest_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (BRF)"]

  rm(boosted_forest_all)

  return(opt_EONR)
}


find_opt_vra_RF_whole_field <- function(entire_data, X_all, Y_all) {
  #--- do tuning ---#
  regression_forest_all <-
    regression_forest(X_all,
      Y_all,
      num.threads = 1,
      tune.parameters = "all",
      num.trees = 1000
    )

  N_data <- data.table(N = seq(min(entire_data$N), max(entire_data$N), by = 1))
  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (RF)"]

  rm(regression_forest_all)

  return(opt_EONR)
}

find_opt_vra_Linear_whole_field <- function(entire_data) {
  N_data <- data.table(N = seq(min(entire_data$N), max(entire_data$N), by = 1))


  linear_all <- lm(yield ~ theta_b2_2 * N + theta_b1_2 * N + Nk_2_1 * N + Nk_2_2 * N + Nk_1_1 * N + Nk_1_2 * N + plateau_2_1 * N + plateau_2_2 * N + plateau_1_1 * N + plateau_1_2 * N + I(N^2),
    data = entire_data
  )


  X_test <-
    entire_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(linear_all, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (Linear)"]

  rm(lienar_all)

  return(opt_EONR)
}

# cf entire data

find_opt_vra_CF_whole_field <- function(data) {
  Y <- data[, yield] #* dependent variable
  W_f <- as.factor(data[, N_tgt]) #* treatment factor variable

  N_levels <- unique(data$N_tgt)

  X <- data[, ..cov_list_cf]

  macf_tau <-
    grf::multi_arm_causal_forest(
      X, Y, W_f,
      num.threads = 1,
      num.trees = 1000
    )

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Predict treatment effects
  # /*+++++++++++++++++++++++++++++++++++
  macf_delta <-
    predict(
      macf_tau,
      newdata = X
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := data[, aunit_id]] %>%
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
    setnames("N_high", "opt_N_hat") %>%
    .[, method := "R-learner (CF)"]

  rm(macf_tau)

  return(macf_results)
}
