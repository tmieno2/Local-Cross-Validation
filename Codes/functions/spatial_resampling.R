spatial_resampling <- function(num_folds, num_repeats = 10, overlap_threshold = 0.8) {

  #* +++++++++++++++++++++++++++++++++++
  #* Spatial resampling (train-test)
  #* +++++++++++++++++++++++++++++++++++
  #* very similar test-train datasets are created in this process
  spatial_folds <-
    spatial_clustering_cv(
      field_au_sf,
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
    rbindlist() %>%
    rowwise() %>%
    mutate(n_in_group = length(group)) %>%
    filter(n_in_group > 1) %>%
    data.table() %>%
    .[, group] %>%
    unique() %>%
    lapply(., function(x) x[-1]) %>%
    unlist() %>%
    unique()

  return_data <-
    spatial_folds[-drop_ids, ] %>%
    .[, .(training_ids, test_ids)] %>%
    .[, split_id := 1:.N]

  # data.table(id = return_data$test_ids %>% reduce(c)) %>%
  #   .[, .N, by = id] %>%
  #   .[order(id), N] %>% hist

  return(return_data)
}
