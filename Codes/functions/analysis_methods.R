#* ===========================================================
#* BRF
#* ===========================================================
# temp <- find_opt_vra_BRF(data, x_vars)

find_opt_vra_BRF <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on BRF")

  Y <- train_data[, yield]
  all_x_vars <- c("N", x_vars)
  X <- train_data[, ..all_x_vars]

  #* +++++++++++++++++++++++++++++++++++
  #* Training
  #* +++++++++++++++++++++++++++++++++++
  brf_trained <-
    boosted_regression_forest(
      X,
      Y,
      num.threads = 1,
      tune.parameters = "all",
      num.trees = 1000
    )

  #* +++++++++++++++++++++++++++++++++++
  #* Find EONR
  #* +++++++++++++++++++++++++++++++++++
  N_data <- data.table(N = seq(min(eval_data$N), max(eval_data$N), by = 1))

  opt_EONR <-
    eval_data[, c(x_vars, "aunit_id"), with = FALSE] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table() %>%
    .[, y_hat := predict(brf_trained, newdata = .[, ..all_x_vars])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (BRF)"]

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    yield_hat_data <-
      eval_data %>%
      .[, yield_hat := predict(brf_trained, newdata = .[, ..all_x_vars])] %>%
      .[, .(aunit_id, yield, yield_hat)]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(brf_trained)
    return(eonr_yield_hat)
  } else {
    rm(brf_trained)
    return(opt_EONR)
  }
}

#* ===========================================================
#* RF
#* ===========================================================
# temp <- find_opt_vra_RF(data, x_vars)

find_opt_vra_RF <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on RF")

  Y <- train_data[, yield]
  all_x_vars <- c("N", x_vars)
  X <- train_data[, ..all_x_vars]

  #* +++++++++++++++++++++++++++++++++++
  #* Train RF
  #* +++++++++++++++++++++++++++++++++++
  rf_trained <-
    regression_forest(
      X,
      Y,
      num.threads = 1,
      tune.parameters = "all",
      num.trees = 1000
    )

  #* +++++++++++++++++++++++++++++++++++
  #* Find EONR
  #* +++++++++++++++++++++++++++++++++++
  N_data <- data.table(N = seq(min(eval_data$N), max(eval_data$N), by = 1))

  opt_EONR <-
    eval_data[, c(x_vars, "aunit_id"), with = FALSE] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table() %>%
    .[, y_hat := predict(rf_trained, newdata = .[, ..all_x_vars])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (RF)"]

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    yield_hat_data <-
      eval_data %>%
      .[, yield_hat := predict(rf_trained, newdata = .[, ..all_x_vars])] %>%
      .[, .(aunit_id, yield, yield_hat)]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(rf_trained)
    return(eonr_yield_hat)
  } else {
    rm(rf_trained)
    return(opt_EONR)
  }
}

#* ===========================================================
#* Linear
#* ===========================================================
find_opt_vra_Linear <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on LM")

  #--- define formula ---#
  control_vars <- paste0(x_vars, collapse = "+")
  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  reg_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  #--- linear regression ---#
  lm_trained <- lm(reg_formula, data = train_data)

  #* +++++++++++++++++++++++++++++++++++
  #* Find EONR
  #* +++++++++++++++++++++++++++++++++++
  N_data <- data.table(N = seq(min(eval_data$N), max(eval_data$N), by = 1))

  opt_EONR <-
    #--- create evaluation data ---#
    eval_data[, c(x_vars, "aunit_id"), with = FALSE] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table() %>%
    .[, N2 := N^2] %>%
    #--- predict yield and profit ---#
    .[, y_hat := predict(lm_trained, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (Linear)"]

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    yield_hat_data <-
      copy(eval_data) %>%
      .[, yield_hat := predict(lm_trained, newdata = .)] %>%
      .[, .(aunit_id, yield, yield_hat)]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(lm_trained)
    return(eonr_yield_hat)
  } else {
    rm(lm_trained)
    return(opt_EONR)
  }
}

#* ===========================================================
#* Causal Forest
#* ===========================================================
find_opt_vra_CF <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on CF")

  Y <- train_data[, yield] #* dependent variable
  W_f <- as.factor(train_data[, N_tgt]) #* treatment factor variable
  N_levels <- unique(train_data$N_tgt)
  X <- train_data[, ..x_vars]

  macf_tau <-
    grf::multi_arm_causal_forest(
      X, Y, W_f,
      num.threads = 1,
      num.trees = 1000
    )

  #* +++++++++++++++++++++++++++++++++++
  #* TE prediction
  #* +++++++++++++++++++++++++++++++++++
  X_eval <- eval_data[, ..x_vars]

  macf_delta <-
    predict(
      macf_tau,
      newdata = X_eval
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := eval_data[, aunit_id]] %>%
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

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    yield_hat_data <-
      copy(eval_data) %>%
      .[, yield_hat := NA] %>%
      .[, .(aunit_id, yield, yield_hat)]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(macf_tau, macf_delta)
    return(eonr_yield_hat)
  } else {
    rm(macf_tau, macf_delta)
    return(opt_EONR)
  }
}

#* ===========================================================
#* Causal Forest (smooth)
#* ===========================================================
find_opt_vra_CF_smooth <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on CF")

  Y <- train_data[, yield] #* dependent variable
  W_f <- as.factor(train_data[, N_tgt]) #* treatment factor variable
  N_levels <- unique(train_data$N_tgt)
  X <- train_data[, ..x_vars]

  #* +++++++++++++++++++++++++++++++++++
  #* Training
  #* +++++++++++++++++++++++++++++++++++
  macf_tau <-
    grf::multi_arm_causal_forest(
      X, Y, W_f,
      num.threads = 1,
      num.trees = 1000
    )

  #* +++++++++++++++++++++++++++++++++++
  #* TE prediction
  #* +++++++++++++++++++++++++++++++++++
  X_eval <- eval_data[, ..x_vars]

  macf_delta <-
    predict(
      macf_tau,
      newdata = X_eval
    )[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := eval_data[, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, c("N", "N_low") := tstrsplit(variable, " - ", fixed = TRUE)] %>%
    .[, .(aunit_id, value, N)] %>%
    .[order(aunit_id), ]

  N_lowest_data <-
    data.table(
      aunit_id = unique(eval_data$aunit_id),
      value = 0,
      N = N_levels[1]
    )

  opt_EONR <-
    rbind(macf_delta, N_lowest_data) %>%
    .[order(aunit_id), ] %>%
    .[, N := as.numeric(N)] %>%
    nest_by(aunit_id) %>%
    mutate(gam_trained = list(
      gam(value ~ s(N, k = 3), data = data)
    )) %>%
    mutate(
      opt_N_hat =
        data.table(N = seq(min(data$N), max(data$N), by = 1)) %>%
          .[, y_hat := predict(gam_trained, newdata = .)] %>%
          .[, pi_hat := pCorn * y_hat - pN * N] %>%
          .[which.max(pi_hat), ] %>%
          .[, N]
    ) %>%
    data.table() %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    .[, method := "R-learner (CF)"]

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    yield_hat_data <-
      copy(eval_data) %>%
      .[, yield_hat := NA] %>%
      .[, .(aunit_id, yield, yield_hat)]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(macf_tau, macf_delta)
    return(eonr_yield_hat)
  } else {
    rm(macf_tau, macf_delta)
    return(opt_EONR)
  }

  return(opt_EONR)
}

#* ===========================================================
#* Spatial error
#* ===========================================================
find_opt_vra_SE <- function(x_vars, train_data, eval_data, yield_pred = TRUE) {
  print("Working on SE")

  #* +++++++++++++++++++++++++++++++++++
  #* Scale the train data
  #* +++++++++++++++++++++++++++++++++++
  scaler_data <-
    train_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
    melt(id.vars = "aunit_id") %>%
    nest_by(variable) %>%
    mutate(scaler = 1 / max(data$value)) %>%
    data.table() %>%
    .[, .(variable, scaler)]

  scaled_data <-
    train_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
    scale_data(., scaler_data)

  #* +++++++++++++++++++++++++++++++++++
  #* Regression
  #* +++++++++++++++++++++++++++++++++++
  control_vars <- paste0(x_vars, collapse = "+")

  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  reg_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  #--- weight matrix ---#
  Wls <- gen_weight_matrix(train_data)

  # === spatial error model ===#
  # ser_trained <- spatialreg::errorsarlm(reg_formula, data = scaled_data, listw = Wls)
  ser_trained <- spatialreg::GMerrorsar(reg_formula, data = scaled_data, listw = Wls)
  # summary(ser_trained)

  #* +++++++++++++++++++++++++++++++++++
  #* Find EONR
  #* +++++++++++++++++++++++++++++++++++
  #* define N space
  N_data <- data.table(N = seq(min(eval_data$N), max(eval_data$N), by = 1))

  data_for_pred <-
    eval_data[, c("yield", "aunit_id", x_vars), with = FALSE] %>%
    #--- prediction data ---
    expand_grid_df(., N_data) %>%
    .[, N2 := N^2] %>%
    scale_data(., scaler_data)

  X_mat_eval <- model.matrix(reg_formula, data = data_for_pred)

  opt_EONR <-
    data_for_pred[, yield := X_mat_eval %*% ser_trained$coef] %>%
    .[, .(yield, N, aunit_id)] %>%
    scale_data(., scaler_data, back = TRUE) %>%
    .[, pi_hat := pCorn * yield - pN * N] %>%
    #--- optimal N ---
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, method := "S-learner (SE)"]

  #* +++++++++++++++++++++++++++++++++++
  #* Yield prediction at the observed points
  #* +++++++++++++++++++++++++++++++++++
  if (yield_pred == TRUE) {
    eval_data_scaled <- scale_data(copy(eval_data), scaler_data)

    X_mat_eval <- model.matrix(reg_formula, data = eval_data_scaled)

    yield_pred <-
      eval_data_scaled[, yield := X_mat_eval %*% ser_trained$coef] %>%
      .[, .(yield, N, aunit_id)] %>%
      scale_data(., scaler_data, back = TRUE) %>%
      .[, .(aunit_id, yield)] %>%
      setnames("yield", "yield_hat")

    yield_hat_data <- eval_data[, .(aunit_id, yield)][yield_pred, on = "aunit_id"]

    eonr_yield_hat <- yield_hat_data[opt_EONR, on = "aunit_id"]

    rm(ser_trained, Wls, X_mat_eval)
    return(eonr_yield_hat)
  } else {
    rm(ser_trained, Wls, X_mat_eval)
    return(opt_EONR)
  }
}

find_opt_ura_gam <- function(x_vars, train_data, eval_data, yield_pred) {
  # x_vars and yield_pred are not used. just there to be consistent with other methods.

  print("Working on GAM")

  gam_trained <- gam(yield ~ s(N, k = 4, m = 2), data = eval_data)

  N_data <- data.table(N = seq(min(eval_data$N), max(eval_data$N), by = 1))

  opt_EONR <-
    copy(N_data) %>%
    .[, y_hat := predict(gam_trained, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N)] %>%
    .[which.max(pi_hat), N]

  # returned data is unnecessarily bulky to be consistent with the other methods
  return_data <-
    eval_data[, .(aunit_id)] %>%
    .[, opt_N_gam := opt_EONR] %>%
    .[, method := "GAM"]

  rm(gam_trained)

  return(return_data)
}
