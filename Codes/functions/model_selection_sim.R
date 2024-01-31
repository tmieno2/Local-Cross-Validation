model_selection_sim <- function(data_files, field_sf, models, num_repeats, num_folds, num_cores, force = FALSE) {

  #--- create a directory to store the results ---#
  print("Creating a directory to store the results.")
  results_dir <- paste0("Shared/Results/sim_results_num_repeats_", num_repeats, "_num_folds_", num_folds)

  if (file.exists(results_dir) & force == FALSE) {
    return(NULL)
  } else {
    dir.create(results_dir)
  }

  #--- create seed for spatial cross-validation ---#
  print("Creating test-train split.")
  train_test_split_file <- here::here(results_dir, "train_test_split.rds")

  if (!file.exists(train_test_split_file)) {
    train_test_split <- spatial_resampling(field_sf, num_folds, num_repeats)
    saveRDS(train_test_split, here::here(results_dir, "train_test_split.rds"))
  } else {
    train_test_split <- readRDS(here::here(results_dir, "train_test_split.rds"))
  }

  #--- simulation ---#
  print("Running simulations")
  pbmclapply(
    1:length(data_files),
    function(x) {
      model_selection_sim_single_field(
        file_path = data_files[x],
        models = models,
        results_dir = results_dir,
        train_test_split = train_test_split
      )
    },
    mc.cores = num_cores,
    mc.preschedule = FALSE
  )
}

model_selection_sim_single_field <- function(file_path, models, results_dir, train_test_split) {
  print(paste0("Working on ", file_path))
  # load the data
  w_data <- readRDS(file_path)

  whole_data <-
    w_data$data[[1]] %>%
    # find true EONR
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)]

  # create spatial data folds
  data_sf <- st_as_sf(whole_data, coords = c("X", "Y"))

  #* +++++++++++++++++++++++++++++++++++
  #* Create folds for cross-validation
  #* +++++++++++++++++++++++++++++++++++
  spatial_folds <-
    train_test_split %>%
    rowwise() %>%
    mutate(training_data = list(
      whole_data[aunit_id %in% training_ids, ]
    )) %>%
    mutate(test_data = list(
      whole_data[aunit_id %in% test_ids, ]
    )) %>%
    data.table() %>%
    .[, .(split_id, training_data, test_data)]

  #* +++++++++++++++++++++++++++++++++++
  #* Find local EONR for each fold by method and predict yield
  #* +++++++++++++++++++++++++++++++++++

  #--- loop over fold ---#
  analysis_results_all_folds <-
    #--- loop over folds ---#
    lapply(
      1:nrow(spatial_folds),
      function(x) analyze_single_fold(x, spatial_folds, models)
    ) %>%
    rbindlist() %>%
    unnest(results) %>%
    #--- add true opt N ---#
    left_join(., whole_data[, .(aunit_id, opt_N)], by = "aunit_id") %>%
    mutate(sim = w_data$sim) %>%
    nest_by(model) %>%
    mutate(data = list(
      data.table(data)
    )) %>%
    data.table()

  #* +++++++++++++++++++++++++++++++++++
  #* Save the results
  #* +++++++++++++++++++++++++++++++++++

  lapply(
    1:nrow(analysis_results_all_folds),
    function(x) {
      model <- analysis_results_all_folds[x, model]
      file_name <- paste0(model, "_sim_", w_data$sim, ".rds")
      saveRDS(
        analysis_results_all_folds[x, data][[1]],
        here::here(results_dir, file_name)
      )
    }
  )

  rm(analysis_results_all_folds, spatial_folds, whole_data, data_sf)
}

#' Find local EONR for a single fold
#'
#' Find local EONR for a single fold. This is looped over all the folds in spatial_folds. In each fold, models specified in the models parameter will be used to find EONR for each of the observations in the test data (evaluation data).
#'
#' @param n index of spatial folds
#' @param spatial_folds resampled observations
#' @param models collection of models applied
#' @returns
analyze_single_fold <- function(n, spatial_folds, models) {
  print(n)

  temp_fold <- spatial_folds[n, ]

  train_data <- temp_fold$training_data[[1]]
  eval_data <- copy(temp_fold$test_data[[1]])

  # models <- c("brf", "rf")

  #* +++++++++++++++++++++++++++++++++++
  #* Find local EONR and predict yield for each evaluation fold
  #* +++++++++++++++++++++++++++++++++++

  local_eonr <-
    data.table(
      model = models
    ) %>%
    rowwise() %>%
    mutate(fcn = list(
      model_picker(model)
    )) %>%
    mutate(results = list(
      fcn(x_vars, train_data, eval_data, yield_pred = TRUE)
    )) %>%
    data.table() %>%
    .[, .(model, results)] %>%
    .[, split_id := temp_fold$split_id]

  return(local_eonr)
}
