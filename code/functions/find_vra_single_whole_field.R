find_vra_single_whole_field <- function(x_vars, models = c("brf", "rf", "lm", "se", "cf"), data_file_path, results_path) {
  #--- load the data ---#
  w_data <- readRDS(data_file_path)
  train_data <- copy(w_data$data[[1]])
  eval_data <- copy(train_data) # since finding eonr for the training data

  #* +++++++++++++++++++++++++++++++++++
  #* True EONR
  #* +++++++++++++++++++++++++++++++++++
  true_eonr <-
    copy(train_data) %>%
    # find true EONR
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>%
    .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
    .[, pi_opt := pCorn * yield_opt - pN * opt_N] %>%
    .[, .(aunit_id, b0, b1, b2, Nk, opt_N, yield_opt, pi_opt)]

  #* +++++++++++++++++++++++++++++++++++
  #* Site-specific EONR for the entire field
  #* +++++++++++++++++++++++++++++++++++

  # cor(vra_eonr$data[[1]]$opt_N_hat, vra_eonr$data[[5]]$opt_N)
  # cor(vra_eonr$data[[2]]$opt_N_hat, vra_eonr$data[[5]]$opt_N)
  # cor(vra_eonr$data[[3]]$opt_N_hat, vra_eonr$data[[5]]$opt_N)
  # cor(vra_eonr$data[[4]]$opt_N_hat, vra_eonr$data[[5]]$opt_N)
  # cor(vra_eonr$data[[5]]$opt_N_hat, vra_eonr$data[[5]]$opt_N)

  vra_eonr <-
    data.table(model = models) %>%
    rowwise() %>%
    mutate(fcn = list(
      model_picker(model)
    )) %>%
    mutate(results = list(
      fcn(
        x_vars = x_vars,
        train_data = train_data,
        eval_data = eval_data,
        yield_pred = FALSE
      )
    )) %>%
    data.table() %>%
    .[, .(model, results)] %>%
    .[, sim := w_data$sim] %>%
    unnest(results) %>%
    data.table() %>%
    true_eonr[., on = "aunit_id"] %>%
    .[, yield_opt_hat := gen_yield_QP(b0, b1, b2, Nk, opt_N_hat)] %>%
    .[, pi_opt_hat := pCorn * yield_opt_hat - pN * opt_N_hat] %>%
    .[, pi_deficit := pi_opt_hat - pi_opt] %>%
    nest_by(model) %>%
    mutate(data = list(
      data.table(data)
    )) %>%
    data.table()

  #* +++++++++++++++++++++++++++++++++++
  #* Save the results
  #* +++++++++++++++++++++++++++++++++++
  lapply(
    1:nrow(vra_eonr),
    function(x) {
      model <- vra_eonr[x, model]
      file_name <- paste0(model, "_sim_", w_data$sim, ".rds")
      saveRDS(
        vra_eonr[x, data][[1]],
        here::here(results_path, file_name)
      )
    }
  )
}
