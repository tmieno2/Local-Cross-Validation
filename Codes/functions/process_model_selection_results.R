process_model_selection_results <- function(num_folds, num_repeats) {
  results_dir <- paste0("Shared/Results/sim_results_num_repeats_", num_repeats, "_num_folds_", num_folds)

  #* +++++++++++++++++++++++++++++++++++
  #* Combine the results
  #* +++++++++++++++++++++++++++++++++++
  #* Read all the simulation results for the main models (except GAM) and combine them together.
  main_results <-
    list.files(
      results_dir,
      full.names = TRUE
    ) %>%
    .[!str_detect(., "train_test|gam")] %>%
    purrr::map(readRDS) %>%
    rbindlist(fill = TRUE)

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
    .[, eonr_rank := seq(.N), by = sim] %>%
    .[, eonr_selected_gam := ifelse(eonr_rank == 1, 1, 0)] %>%
    .[order(sim, rmse_yield), ] %>%
    #--- within-sim rank based on yield prediction ---#
    .[, yield_rank := seq(.N), by = sim] %>%
    .[, yield_selected := ifelse(yield_rank == 1, 1, 0)]

  #* +++++++++++++++++++++++++++++++++++
  #* Combine with the true rank data
  #* +++++++++++++++++++++++++++++++++++
  comp_ranks_data <-
    selection_ranks[sum_results_whole, on = c("sim", "method")]

  return(comp_ranks_data)
}
