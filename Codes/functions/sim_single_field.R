sim_single_field <- function(i, raw_data, num_repeats, num_folds) {
  print(paste0("Working on field ", i))
  # load the data
  data <-
    raw_data[3, ]$reg_data[[1]]$data[[i]] %>%
    # find true EONR
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)]

  # create spatial data folds
  data_sf <- st_as_sf(data, coords = c("X", "Y"))

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Create folds for cross-validation
  # /*+++++++++++++++++++++++++++++++++++
  num_it <- num_repeats * num_folds

  spatial_folds <- lapply(1:num_repeats, function(n) {
    skcv_folds_try <- spatial_clustering_cv(data_sf, v = num_folds)

    scv_try <-
      skcv_folds_try %>%
      rowwise() %>%
      mutate(
        training_data = list(analysis(splits) %>% data.table()),
        test_data = list(assessment(splits) %>% data.table())
      ) %>%
      mutate(repeats = n)

    return(scv_try)
  }) %>% rbindlist()

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find local EONR for each fold by method and predict yield
  # /*+++++++++++++++++++++++++++++++++++
  analysis_results_folds <-
    lapply(
      1:num_it,
      function(x) analyze_local_all(x, spatial_folds, data)
    )

  #--- RMSE of local EONR (for model selection) ---#
  rmse_local_eonr <-
    purrr::map(analysis_results_folds, "local_eonr_data") %>%
    rbindlist() %>%
    .[, eonr_dif := gam_opt_N_hat - local_opt_N_hat] %>%
    .[, .(rmse_local_eonr = sqrt(mean(eonr_dif^2))), by = method]

  #--- RMSE of yield prediction ---#
  rmse_yield <-
    purrr::map(analysis_results_folds, "yield_hat_data") %>%
    rbindlist() %>%
    .[, yield_dif := yield - yield_hat] %>%
    .[, .(rmse_yield = sqrt(mean(yield_dif^2))), by = method]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find site-specific (vra) optimal EONR by method
  # /*+++++++++++++++++++++++++++++++++++
  vra_eonr_hat <- find_vra_whole_all(data)

  #--- Site-specific EONR (for validation) ---#
  rmse_eonr_whole <-
    vra_eonr_hat %>%
    data[, .(aunit_id, opt_N)][., on = "aunit_id"] %>%
    .[, .(rmse_eonr = sqrt(mean((opt_N_hat - opt_N)^2))), by = method]

  data_to_return <-
    rmse_local_eonr %>%
    rmse_yield[., on = "method"] %>%
    rmse_eonr_whole[., on = "method"] %>%
    .[, field_number := i]

  return(data_to_return)
}
