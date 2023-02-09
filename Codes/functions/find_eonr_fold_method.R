analyze_local_all <- function(n, spatial_folds, data) {
  print(n)

  temp_fold <- spatial_folds[n, ]

  train_data <- temp_fold$training_data[[1]]
  test_data <- temp_fold$test_data[[1]]

  X <- as.matrix(train_data[, cov_list, with = FALSE])
  Y <- train_data$yield
  W_f <- as.factor(train_data[, N_tgt]) #* treatment factor variable
  X_cf_train <- train_data[, ..cov_list_cf]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  results_BRF <- analyze_local_BRF(data, X, Y, test_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## RF
  # /*+++++++++++++++++++++++++++++++++++
  results_RF <- analyze_local_RF(data, X, Y, test_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Linear
  # /*+++++++++++++++++++++++++++++++++++
  results_Linear <- analyze_local_Linear(data, test_data, train_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## GAM
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_gam <- analyze_local_gam(data, test_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## CF
  # /*+++++++++++++++++++++++++++++++++++
  results_CF <- analyze_local_CF(data, X_cf_train, Y, W_f, test_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  local_eonr_data <-
    rbind(
      results_BRF$opt_EONR,
      results_RF$opt_EONR,
      results_Linear$opt_EONR,
      results_CF$opt_EONR
    ) %>%
    .[, .(local_opt_N_hat = mean(opt_N_hat)), by = method] %>%
    .[, id := temp_fold$id] %>%
    .[, repeats := temp_fold$repeats] %>%
    .[, gam_opt_N_hat := opt_EONR_gam$opt_N_hat]

  yield_hat_data <-
    rbind(
      results_BRF$yield_hat_data,
      results_RF$yield_hat_data,
      results_Linear$yield_hat_data
    ) %>%
    .[, id := temp_fold$id] %>%
    .[, repeats := temp_fold$repeats]

  return(list(
    local_eonr_data = local_eonr_data,
    yield_hat_data = yield_hat_data
  ))
}

################################################################################
# ^^^^^^^^^^^^^^^^^^^ EONR based selection functions^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################
# analyze_local_BRF(data, X, Y, test_data)

analyze_local_BRF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y, tune.parameters = "all", num.trees = 1000)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## EONR prediction
  # /*+++++++++++++++++++++++++++++++++++
  N_data <- data.table(N = seq(min(data$N), max(data$N), by = 1))

  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(boosted_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    setnames("N", "opt_N_hat") %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    .[, method := "S-learner (BRF)"]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## yield prediction
  # /*+++++++++++++++++++++++++++++++++++
  yield_hat_data <-
    test_data %>%
    .[, yield_hat := predict(boosted_forest, newdata = .[, ..cov_list])] %>%
    .[, .(aunit_id, yield, yield_hat)] %>%
    .[, method := "S-learner (BRF)"]

  return(list(opt_EONR = opt_EONR, yield_hat_data = yield_hat_data))
}

# analyze_local_RF(data, X, Y, test_data)
analyze_local_RF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y, tune.parameters = "all", num.trees = 1000)

  N_data <- data.table(N = seq(min(data$N), max(data$N), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    setnames("N", "opt_N_hat") %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    .[, method := "S-learner (RF)"]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## yield prediction
  # /*+++++++++++++++++++++++++++++++++++
  yield_hat_data <-
    test_data %>%
    .[, yield_hat := predict(regression_forest, newdata = .[, ..cov_list])] %>%
    .[, .(aunit_id, yield, yield_hat)] %>%
    .[, method := "S-learner (RF)"]

  return(list(opt_EONR = opt_EONR, yield_hat_data = yield_hat_data))
}

# analyze_local_Linear(data, test_data, train_data)

analyze_local_Linear <- function(data, test_data, train_data) {
  N_data <- data.table(N = seq(min(data$N), max(data$N), by = 1))

  linear_trained <- lm(yield ~ theta_b2_2 * N + theta_b2_1 * N + theta_b1_2 * N + theta_b1_1 * N + Nk_2_1 * N + Nk_2_2 * N + Nk_1_1 * N + Nk_1_2 * N + plateau_2_1 * N + plateau_2_2 * N + plateau_1_1 * N + plateau_1_2 * N + I(N^2),
    data = train_data
  )

  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(linear_trained, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    setnames("N", "opt_N_hat") %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    .[, method := "S-learner (Linear)"]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## yield prediction
  # /*+++++++++++++++++++++++++++++++++++
  yield_hat_data <-
    test_data %>%
    .[, yield_hat := predict(linear_trained, newdata = .[, ..cov_list])] %>%
    .[, .(aunit_id, yield, yield_hat)] %>%
    .[, method := "S-learner (Linear)"]

  return(list(opt_EONR = opt_EONR, yield_hat_data = yield_hat_data))
}

analyze_local_gam <- function(data, test_data) {
  N_data <- data.table(N = seq(min(data$N), max(data$N), by = 1))

  gam <- gam(yield ~ s(N, k = 4, m = 2), data = test_data)

  opt_EONR <-
    copy(N_data) %>%
    .[, y_hat := predict(gam, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ]] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "GAM"]

  return(opt_EONR)
}

analyze_local_CF <- function(data, X_cf_train, Y, W_f, test_data) {
  X_cv <- test_data[, ..cov_list_cf]

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
    setnames("N_high", "opt_N_hat") %>%
    .[, method := "R-learner (CF)"]

  return(list(opt_EONR = opt_EONR, yield_hat_data = NULL))
}
