find_vra_single_whole_field <- function(file_path, x_vars, models = c("brf", "rf", "lm", "se", "cf")) {
  print(paste0("Working on ", file_path))

  #--- load the data ---#
  w_data <- readRDS(file_path)
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
    .[, .(aunit_id, opt_N)]

  #* +++++++++++++++++++++++++++++++++++
  #* Site-specific EONR for the entire field
  #* +++++++++++++++++++++++++++++++++++
  vra_eonr <-
    data.table(
      model = models
    ) %>%
    rowwise() %>%
    mutate(fcn = list(
      model_picker(model)
    )) %>%
    mutate(results = list(
      fcn(x_vars, train_data, eval_data, yield_pred = FALSE)
    )) %>%
    data.table() %>%
    .[, .(model, results)] %>%
    .[, sim := w_data$sim] %>%
    unnest(results) %>%
    data.table() %>%
    true_eonr[., on = "aunit_id"] %>%
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
        here::here("Shared/Results/WholeField/", file_name)
      )
    }
  )
}
