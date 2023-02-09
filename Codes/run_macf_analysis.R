# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis
# Debugging instruction
# 1. run all the chunks in 1_simulation.Rmd except the last one.
# 2. run source(here("GitControlled/Codes/Functions/prepare_debug_data.R"))

train_data <- copy(reg_data)
test_data <- copy(cv_data$data[[1]])

Y <- train_data[, yield] #* dependent variable
W_f <- as.factor(train_data[, N_tgt]) #* treatment factor variable

x_vars_exp <- paste0(x_vars, collapse = ",")
eval(parse(text = paste0("X = train_data[, .(", x_vars_exp, ")]"))) #* X
eval(parse(text = paste0("X_cv = test_data[, .(", x_vars_exp, ")]"))) #* X for test data

macf_tau <-
  grf::multi_arm_causal_forest(
    X, Y, W_f,
    num.threads = 1
  )

# /*+++++++++++++++++++++++++++++++++++
#' ## Select variables to include
# /*+++++++++++++++++++++++++++++++++++
#* This process does not really help
# var_imp <- grf::variable_importance(macf_tau)
# keep <- var_imp > (mean(var_imp) / 2)

# x_vars <- x_vars[keep]
# X <- data[, x_vars, with = FALSE]
# X_cv <- cv_data$data[[1]][, x_vars, with = FALSE]

# macf_tau <-
#   grf::multi_arm_causal_forest(
#     X, Y, W_f,
#     num.threads = 1
#   )

# /*+++++++++++++++++++++++++++++++++++
#' ## Predict treatment effects
# /*+++++++++++++++++++++++++++++++++++
macf_delta <-
  predict(
    macf_tau,
    newdata = X_cv
  )[[1]][, , 1] %>%
  data.table() %>%
  .[, aunit_id := train_data[, aunit_id]] %>%
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
  #--- RMSE not exist ---
  .[, e_hat_train := 0] %>%
  .[, e_hat_cv := 0] %>%
  .[, sim := cv_data$sim]
