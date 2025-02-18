#' Run model selection
#'
#' Conduct model selection based on yield and local EONR for all the synthetic fields for a given fold-repeat combination
#'
#' @param data_files list of file paths that specify the location of the files that hold simualtion data
#' @param field_sf an sf object of the field (common for all the fields)
#' @param models vector of models
#' @param num_repeats number of repeats
#' @param num_folds number of folds
#' @param num_cores number of cores used for parallelized computation
#' @param results_root_dir root directory where the model selection results are saved
#' @returns
model_selection_sim <- function(data_files, field_sf, models, num_repeats, num_folds, num_cores, results_root_dir, force = FALSE) {
  #++++++++++++++++++++++++++++++++++++
  #+ create a directory to store the results
  #++++++++++++++++++++++++++++++++++++

  results_dir <-
    file.path(
      results_root_dir,
      paste0(
        "sim_results_num_repeats_", num_repeats,
        "_num_folds_", num_folds
      )
    )

  if (file.exists(results_dir) & force == FALSE) {
    stop("The results seem to exist. If you would like to force running model selection simulations, please set force == TRUE")
  } else if (!file.exists(results_dir)) {
    print("Creating a directory to store the results.")
    dir.create(results_dir)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ create seed for spatial cross-validation
  #++++++++++++++++++++++++++++++++++++
  print("Creating test-train split.")
  train_test_split_file <- here::here(results_dir, "train_test_split.rds")

  if (!file.exists(train_test_split_file)) {
    train_test_split <-
      spatial_resampling(
        data_sf = field_sf,
        num_folds = num_folds,
        num_repeats = num_repeats
      )
    saveRDS(train_test_split, here::here(results_dir, "train_test_split.rds"))
  } else {
    train_test_split <- readRDS(here::here(results_dir, "train_test_split.rds"))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Model selection
  #++++++++++++++++++++++++++++++++++++
  print("Implementing model selection")
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

#' Run model selection for a single field (Internal)
#'
#' Conduct model selection based on yield and local EONR for a single fold-repeat combination. It uses analyze_single_fold() internally.
#'
#' @param file_path file path to the file that holds the simualtion data for the field
#' @param models vector of models
#' @param results_dir root directory where the model selection results are saved
#' @param train_test_split observation IDs for the train and test data
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
  print(paste0("Fold: ", n))

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

spatial_resampling <- function(data_sf, num_folds, num_repeats = 10, overlap_threshold = 0.8) {
  #* +++++++++++++++++++++++++++++++++++
  #* Spatial resampling (train-test)
  #* +++++++++++++++++++++++++++++++++++
  #* very similar test-train datasets are created in this process
  spatial_folds <-
    spatial_clustering_cv(
      data_sf,
      v = num_folds,
      repeats = num_repeats
    ) %>%
    rowwise() %>%
    mutate(
      training_ids = list(analysis(splits) %>% data.table() %>% .[, aunit_id]),
      test_ids = list(assessment(splits) %>% data.table() %>% .[, aunit_id])
    ) %>%
    data.table() %>%
    # setnames(c("id", "id2"), c("repeats", "folds")) %>%
    .[, .(training_ids, test_ids)]

  #* +++++++++++++++++++++++++++++++++++
  #* Drop sets that are too similar
  #* +++++++++++++++++++++++++++++++++++
  num_it <- nrow(spatial_folds)

  #--- check similarity ---#
  # check the number of overlapping observations
  similar_test_mat <- matrix(0, num_it, num_it)
  for (i in 1:num_it) {
    for (j in 1:num_it) {
      duplicated_nums <-
        mean(spatial_folds[, test_ids][[i]] %in% spatial_folds[, test_ids][[j]])
      similar_test_mat[i, j] <- duplicated_nums
    }
  }

  diag(similar_test_mat) <- 0

  #* ---------------------
  #* identify the row numbers that should be dropped
  #* ---------------------
  drop_ids <-
    lapply(
      1:nrow(spatial_folds),
      function(x) {
        sim_rows <- which(similar_test_mat[, x] > overlap_threshold)
        drop_or_not <- which(similar_test_mat[x, sim_rows] > overlap_threshold)
        group <- c(x, sim_rows[drop_or_not]) %>% .[order(.)]
        return(data.table(group = list(group)))
      }
    ) %>%
    data.table::rbindlist() %>%
    rowwise() %>%
    dplyr::mutate(n_in_group = length(group)) %>%
    dplyr::filter(n_in_group > 1) %>%
    data.table() %>%
    .[, group] %>%
    unique() %>%
    lapply(., function(x) x[-1]) %>%
    unlist() %>%
    unique()

  if (is.null(drop_ids)) {
    return_data <-
      spatial_folds %>%
      .[, .(training_ids, test_ids)] %>%
      .[, split_id := 1:.N]
  } else {
    return_data <-
      spatial_folds[-drop_ids, ] %>%
      .[, .(training_ids, test_ids)] %>%
      .[, split_id := 1:.N]
  }

  # data.table(id = return_data$test_ids %>% reduce(c)) %>%
  #   .[, .N, by = id] %>%
  #   .[order(id), N] %>% hist

  return(return_data)
}
