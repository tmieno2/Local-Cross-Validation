process_model_selection_results <- function(num_folds, num_repeats, results_root_dir) {
  # num_folds <- 7
  # num_repeats <- 10

  results_dir <- file.path(
    results_root_dir,
    paste0(
      "sim_results_num_repeats_", num_repeats, "_num_folds_", num_folds
    )
  )
  #* +++++++++++++++++++++++++++++++++++
  #* Gam results
  #* +++++++++++++++++++++++++++++++++++

  #* ---------------------
  #* Combine
  #* ---------------------
  gam_results <-
    list.files(
      results_dir,
      full.names = TRUE
    ) %>%
    .[str_detect(., "gam")] %>%
    purrr::map(readRDS) %>%
    rbindlist(fill = TRUE)

  #* ---------------------
  #* Summarize
  #* ---------------------
  sum_results_gam <-
    gam_results %>%
    #--- average opt_N by fold ---#
    # opt_N_gam is the same for all the observations
    # within a fold
    .[, .(opt_N_gam = mean(opt_N_gam), opt_N = mean(opt_N)), by = .(sim, split_id)] %>%
    .[, opt_N_dif := opt_N - opt_N_gam] %>%
    .[, rmse_local_eonr := sqrt(mean(opt_N_dif^2)), by = sim]

  #* +++++++++++++++++++++++++++++++++++
  #* Main models
  #* +++++++++++++++++++++++++++++++++++
  #* ---------------------
  #* Combine the results
  #* ---------------------
  #* Read all the simulation results for the main models (except GAM) and combine them together.
  main_results <-
    list.files(
      results_dir,
      full.names = TRUE
    ) %>%
    .[!str_detect(., "train_test|gam")] %>%
    purrr::map(readRDS) %>%
    rbindlist(fill = TRUE)

  #* ---------------------
  #* Summrize
  #* ---------------------
  #* Calculate rmse of EONR and Yield by method. RMSE of EONR here is against true EONR, which is not used for selection in our approach as true EONR is not observed.
  main_sum_results <-
    main_results %>%
    .[, N_dif := opt_N_hat - opt_N] %>%
    .[, yield_dif := yield_hat - yield] %>%
    .[, .(
      #--- rmse by fold ---#
      rmse_eonr_true = sqrt(mean(N_dif^2)),
      rmse_yield = sqrt(mean(yield_dif^2)),
      #--- average estimated EONR ---#
      opt_N_hat = mean(opt_N_hat)
    ),
    by = .(sim, method, split_id)
    ] %>%
    .[order(sim), ]

  #* +++++++++++++++++++++++++++++++++++
  #* Combine the main and gam results
  #* +++++++++++++++++++++++++++++++++++

  #* Combines the gam results and main results, which lets us calculate RMSE of eonr prediction against GAM-estimated local eonr prediction.

  combined_results <-
    sum_results_gam[main_sum_results, on = c("sim", "split_id")] %>%
    .[, opt_N_dif_select := opt_N_hat - opt_N_gam] %>%
    .[, .(
      rmse_eonr_gam = sqrt(mean(opt_N_dif_select^2)),
      rmse_yield = mean(rmse_yield)
    ), by = .(sim, method)]

  #* For each simnulation round, find the ranking of models by RMSE of EONR (against GAM) and yield. The model ranked first is the one selected.

  selection_ranks <-
    combined_results %>%
    .[order(sim, rmse_eonr_gam), ] %>%
    #--- within-sim rank based on local EONR prediction ---#
    .[, eonr_rank_gam := seq(.N), by = sim] %>%
    .[, eonr_selected_gam := ifelse(eonr_rank_gam == 1, 1, 0)] %>%
    .[order(sim, rmse_yield), ] %>%
    #--- within-sim rank based on yield prediction ---#
    .[, yield_rank := seq(.N), by = sim] %>%
    .[, yield_selected := ifelse(yield_rank == 1, 1, 0)]

  #* +++++++++++++++++++++++++++++++++++
  #* Combine with the true rank data
  #* +++++++++++++++++++++++++++++++++++

  comp_summary <-
    selection_ranks[sum_results_whole, on = c("sim", "method")]

  return(comp_summary)
}

# How similar rmse_eonr is between the selected and the actual best

find_loss_from_selection <- function(comp_summary) {
  eonr_selected <-
    comp_summary[eonr_rank_gam == 1, .(sim, method, pi_deficit, rmse_eonr_true)] %>%
    setnames(
      c("method", "pi_deficit", "rmse_eonr_true"),
      c("e_selected_method", "e_selected_pi_deficit", "e_selected_rmse_eonr_true")
    )

  yield_selected <-
    comp_summary[yield_rank == 1, .(sim, method, pi_deficit, rmse_eonr_true)] %>%
    setnames(
      c("method", "pi_deficit", "rmse_eonr_true"),
      c("y_selected_method", "y_selected_pi_deficit", "y_selected_rmse_eonr_true")
    )

  eonr_true_best <- comp_summary[profit_rank_true == 1, .(sim, method, pi_deficit, rmse_eonr_true)]

  loss_data <-
    eonr_true_best %>%
    eonr_selected[, on = "sim"] %>%
    yield_selected[, on = "sim"] %>%
    .[, e_rmse_eonr_loss := e_selected_rmse_eonr_true - rmse_eonr_true] %>%
    .[, e_pi_loss := pi_deficit - e_selected_pi_deficit] %>%
    .[, y_rmse_eonr_loss := y_selected_rmse_eonr_true - rmse_eonr_true] %>%
    .[, y_pi_loss := pi_deficit - y_selected_pi_deficit] %>%
    .[, .(sim, e_rmse_eonr_loss, e_pi_loss, y_rmse_eonr_loss, y_pi_loss)]

  return(loss_data)
}
