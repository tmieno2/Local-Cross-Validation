cov_list <- c(
  "N_tgt", "theta_b2_2", "theta_b2_1", "theta_b1_2", "theta_b1_1", "Nk_2_1", "Nk_2_2",
  "Nk_1_1", "Nk_1_2", "plateau_2_1", "plateau_2_2", "plateau_1_1", "plateau_1_2"
)


cov_list_cf <- c(
  "theta_b2_2", "theta_b2_1", "theta_b1_2", "theta_b1_1", "Nk_2_1", "Nk_2_2",
  "Nk_1_1", "Nk_1_2", "plateau_2_1", "plateau_2_2", "plateau_1_1", "plateau_1_2"
)

x_vars_exp <- paste0(cov_list_cf, collapse = ",")

read_data <- readRDS(here("all_sim_data.rds"))

# Import corn and N price

pCorn <- readRDS(here("pCorn.rds"))
pN <- readRDS(here("pN.rds"))


Local_EONR_each_field <- function(i){
  
  #load the data
  data <- read_data[3, ]$reg_data[[1]]$data[[i]]
  
  #find true EONR
  
  data<- data %>%
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)]
  
  #create spatial data folds
  
  data_sf <- st_as_sf(data, coords = c("X", "Y"))
  
  
  spatial_folds <- lapply(1:10, function(n) {
    skcv_folds_try <- spatial_clustering_cv(data_sf, v = 6)
    
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
  
#lapply(1:60, function(x) find_local_EONR_all(x, spatial_folds, data))
  
  find_local_EONR_all <- function(n, spatial_folds, data) {
    temp_fold <- spatial_folds[n, ]
    
    train_data <- temp_fold$training_data[[1]]
    test_data <- temp_fold$test_data[[1]]
    
    X <- as.matrix(train_data[, cov_list, with = FALSE])
    Y <- train_data$yield
    W_f <- as.factor(train_data[, N_tgt]) #* treatment factor variable
    eval(parse(text = paste0("X_cf_train = train_data[, .(", x_vars_exp, ")]"))) #* X
    
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## BRF
    # /*+++++++++++++++++++++++++++++++++++
    opt_EONR_BRF <-
      find_local_EONR_BRF(data, X, Y, test_data) %>%
      .[, .(N_tgt = mean(N_tgt))] %>%
      .[, method := "S-learner (BRF)"]
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## RF
    # /*+++++++++++++++++++++++++++++++++++
    opt_EONR_RF <-
      find_local_EONR_RF(data, X, Y, test_data) %>%
      .[, .(N_tgt = mean(N_tgt))] %>%
      .[, method := "S-learner (RF)"]
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## Linear
    # /*+++++++++++++++++++++++++++++++++++
    opt_EONR_Linear <-
      find_local_EONR_Linear(data, test_data, train_data) %>%
      .[, .(N_tgt = mean(N_tgt))] %>%
      .[, method := "S-learner (Linear)"]
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## GAM
    # /*+++++++++++++++++++++++++++++++++++
    opt_EONR_gam <-
      find_local_EONR_gam(data, test_data) %>%
      .[, .(N_tgt = N_tgt)] %>%
      .[, method := "S-learner (GAM)"]
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## CF
    # /*+++++++++++++++++++++++++++++++++++
    opt_EONR_CF <-
      find_local_EONR_CF(data, X_cf_train, Y, W_f, test_data) %>%
      .[, .(N_tgt = mean(opt_N_hat))] %>%
      .[, method := "CF_model_selection"]
    
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## Combine
    # /*+++++++++++++++++++++++++++++++++++
    mean_eonr_data <-
      rbind(opt_EONR_BRF, opt_EONR_RF, opt_EONR_Linear, opt_EONR_gam, opt_EONR_CF) %>%
      .[, id := temp_fold$id] %>%
      .[, repeats := temp_fold$repeats]
    
    return(mean_eonr_data)
  } 
  
  EONRs_all_models <- lapply(1:60, function(x) find_local_EONR_all(x, spatial_folds, data)) %>%
    rbindlist()
  
  Find_MSE_Local_EONR <- function(method1, method2, data) {
    mse <- data %>%
      filter(method %in% c(method1, method2)) %>%
      group_by(repeats, id) %>%
      summarise(mse = (diff(N_tgt))^2) %>%
      subset(., select = -c(id)) %>%
      group_by(repeats) %>%
      summarise(mse_each_repeat = sum(mse))
    
    final_mse <- mean(mse$mse_each_repeat)
  }
  ################################################################################
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^MSE of EONR^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ################################################################################  
  mse_of_EONR_gam_BRF <- Find_MSE_Local_EONR("S-learner (BRF)", "S-learner (GAM)", EONRs_all_models)%>%  data.table()%>%
    rename(., mse=.) %>% .[, method := "BRF_EONR"] %>% .[, selection_criterion := "EONR"]
  

  mse_of_EONR_gam_RF <- Find_MSE_Local_EONR("S-learner (RF)", "S-learner (GAM)", EONRs_all_models) %>%  data.table()%>%
    rename(., mse=.) %>% .[, method := "RF_EONR"] %>% .[, selection_criterion := "EONR"]
  
  mse_of_EONR_gam_Linear <- Find_MSE_Local_EONR("S-learner (Linear)", "S-learner (GAM)", EONRs_all_models) %>%  data.table()%>%
    rename(., mse=.) %>% .[, method := "Linear_EONR"] %>% .[, selection_criterion := "EONR"]
  
  mse_of_EONR_gam_CF <- Find_MSE_Local_EONR("CF_model_selection", "S-learner (GAM)", EONRs_all_models)%>%  data.table()%>%
    rename(., mse=.) %>% .[, method := "CF_EONR"] %>% .[, selection_criterion := "EONR"]
  

predict_yield_for_yield_based_model_selection <- function(n, spatial_folds, data) {
  temp_fold <- spatial_folds[n, ]
  
  train_data <- temp_fold$training_data[[1]]
  test_data <- temp_fold$test_data[[1]]
  
  X <- as.matrix(train_data[, cov_list, with = FALSE])
  Y <- train_data$yield
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  mse_brf <- yield_prediction_brf(data, X, Y, test_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (BRF)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## RF
  # /*+++++++++++++++++++++++++++++++++++
  mse_rf <- yield_prediction_rf(data, X, Y, test_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (RF)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Linear
  # /*+++++++++++++++++++++++++++++++++++
  mse_linear <- yield_prediction_linear(data, test_data, train_data) %>%
    .[, .(mse = sum(diff_y_and_y_hat_squared))] %>%
    .[, method := "mse (Linear)"]
  
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  mse_eonr_data <-
    rbind(mse_brf, mse_rf, mse_linear) %>%
    .[, id := temp_fold$id] %>%
    .[, repeats := temp_fold$repeats]
  
  return(mse_eonr_data)
  
  
}

mse_of_yield_all_models_folds_and_repeats <- lapply(1:60, function(x) predict_yield_for_yield_based_model_selection(x, spatial_folds, data)) %>%
  rbindlist()

#Final mse of yield (for each model sum folds and then average over repeats)

Find_MSE_of_yield <- function(method1, data) {
  mse <- data %>%
    filter(method %in% c(method1)) %>%
    group_by(repeats) %>%
    summarise(mse_sum_over_folds = sum(mse)) 
  
  final_mse <- mean(mse$mse_sum_over_folds)
  
} 


################################################################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^MSE of yield^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
################################################################################
mse_of_yield_BRF <- Find_MSE_of_yield("mse (BRF)", mse_of_yield_all_models_folds_and_repeats)%>%  data.table()%>%
  rename(., mse=.) %>%
  .[, method := "BRF_yield"]  %>% .[, selection_criterion := "yield"]

mse_of_yield_RF <- Find_MSE_of_yield("mse (RF)", mse_of_yield_all_models_folds_and_repeats) %>%  data.table()%>% 
  rename(., mse=.) %>%
  .[, method := "RF_yield"] %>% .[, selection_criterion := "yield"]

mse_of_yield_Linear <- Find_MSE_of_yield("mse (Linear)", mse_of_yield_all_models_folds_and_repeats) %>%  data.table()%>% 
  rename(., mse=.) %>%
  .[, method := "Linear_yield"] %>% .[, selection_criterion := "yield"]




#Local EONR train on the entire data

find_local_EONR_train_on_the_entire_data_all <- function(data) {
  
  
  X_all <- as.matrix(data[, cov_list, with = FALSE])
  Y_all <- data$yield
  
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## BRF
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_BRF_entire_data <-
    find_local_EONR_BRF_train_on_the_entire_data(data, X_all, Y_all) %>%
    .[, .(N_tgt = N_tgt)] %>%
    .[, method := "BRF"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## RF
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_RF_entire_data <-
    find_local_EONR_RF_train_on_the_entire_data(data, X_all, Y_all) %>%
    .[, .(N_tgt = N_tgt)] %>%
    .[, method := "RF"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Linear
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_Linear_entire_data <-
    find_local_EONR_Linear_train_on_the_entire_data(data) %>%
    .[, .(N_tgt = N_tgt)] %>%
    .[, method := "Linear"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## CF
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_CF_entire_data <-
    find_local_EONR_CF_train_on_the_entire_data(data) %>%
    .[, .(N_tgt = opt_N_hat)] %>%
    .[, method := "CF"]
  
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  eonr_data <-
    rbind(opt_EONR_BRF_entire_data, opt_EONR_RF_entire_data, opt_EONR_Linear_entire_data, opt_EONR_CF_entire_data) 
  
  return(eonr_data)
} 

EONRs_all_models_train_on_the_entire_data <- find_local_EONR_train_on_the_entire_data_all(data)



Find_MSE_of_EONR_train_on_whole_data <- function(method1, data_trained_on_the_whole) {
  mse <- data_trained_on_the_whole %>%
    filter(method %in% c(method1)) %>%
    mutate(mse= (data$opt_N-N_tgt)^2)
  
  final_mse <- sum(mse$mse)  
  
} 

################################################################################
#^^^^^^^^^^^MSE of the true EONR vs EONR trained on the entire data^^^^^^^^^^^^^^
################################################################################



mse_of_EONR_BRF_train_on_whole_data <- Find_MSE_of_EONR_train_on_whole_data("BRF",EONRs_all_models_train_on_the_entire_data )%>%  
  data.table() %>% rename(., mse=.) %>% .[, method := "BRF_trained_on_entire_data"]  %>% .[, selection_criterion := "truly the best model (EONR)"]

mse_of_EONR_RF_train_on_whole_data <- Find_MSE_of_EONR_train_on_whole_data("RF",EONRs_all_models_train_on_the_entire_data )%>%  
  data.table() %>% rename(., mse=.) %>% .[, method := "RF_trained_on_entire_data"]  %>% .[, selection_criterion := "truly the best model (EONR)"]

mse_of_EONR_Linear_train_on_whole_data <- Find_MSE_of_EONR_train_on_whole_data("Linear",EONRs_all_models_train_on_the_entire_data )%>%  
  data.table() %>% rename(., mse=.) %>% .[, method := "Linear_trained_on_entire_data"]  %>% .[, selection_criterion := "truly the best model (EONR)"]

mse_of_EONR_CF_train_on_whole_data <- Find_MSE_of_EONR_train_on_whole_data("CF",EONRs_all_models_train_on_the_entire_data )%>%  
  data.table() %>% rename(., mse=.) %>% .[, method := "CF_trained_on_entire_data"]  %>% .[, selection_criterion := "truly the best model (EONR)"]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ********************  CF 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cf_whole_data_results <- find_local_EONR_CF_train_on_the_entire_data(data)
# 
# cf_mse <- cf_whole_data_results %>% mutate(mse=(opt_N_hat-data$opt_N)^2)
# 
# final_mse_CF <- sum(cf_mse$mse)  %>% data.table()%>%
#   rename(., mse=.) %>% .[, method := "CF_entire_data"] %>% .[, selection_criterion := "truly the best model (EONR)"]







data_to_return <- rbind(mse_of_yield_BRF, mse_of_yield_RF, mse_of_yield_Linear, mse_of_EONR_gam_BRF,
                        mse_of_EONR_gam_RF, mse_of_EONR_gam_Linear, mse_of_EONR_gam_CF, mse_of_EONR_BRF_train_on_whole_data, 
                        mse_of_EONR_RF_train_on_whole_data, mse_of_EONR_Linear_train_on_whole_data, mse_of_EONR_CF_train_on_whole_data) %>% mutate(field_number= {i}) 
# %>% filter(. , field_number == i )%>%
#   .[ , .SD[which.min(mse)], by = selection_criterion]

  return(data_to_return)
}



########################################## 
 #   test
########################################## 
 

test1 <- lapply(1:2, Local_EONR_each_field )
test2 <- test1 %>% rbindlist()

test3 <- lapply(1:2, Local_EONR_each_field ) %>%rbindlist()
 t <- test3 %>%rbindlist()

test_5_fielsd <- lapply(1:5, Local_EONR_each_field ) %>%rbindlist()


test_with_cf <- lapply(1:4, Local_EONR_each_field ) %>%rbindlist()

test_keep_all_models <- lapply(1:4, Local_EONR_each_field ) %>%rbindlist()

n <- format(test_keep_all_models, scientific = FALSE)



results_test <- lapply(1:2, Local_EONR_each_field ) %>%rbindlist()

m <-  format(results_test, scientific = FALSE)

fifty_fields <- mclapply(1:50, Local_EONR_each_field,mc.cores = detectCores() - 1  )

fifty <- fifty_fields %>%rbindlist()

save(fifty, file = "fifty_fiels_simu.RData")

ff <- format(fifty, scientific = FALSE)

Second_fifty_fiels <- mclapply(51:100, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()
mm <- format(Second_fifty_fiels, scientific = FALSE)

third_fifty_fields <- mclapply(101:150, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()
mmm <- format(third_fifty_fields, scientific = FALSE)

forth_fifty_fields <- mclapply(151:200, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()
mmmm <- format(forth_fifty_fields, scientific = FALSE)

fifth_fifty_plus_fields <- mclapply(201:300, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()

twenty_after_three_hundred <- mclapply(301:320, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()
dd <- twenty_after_three_hundred %>% format(., scientific = FALSE)

mmmmm <- format(fifth_fifty_plus_fields, scientific = FALSE)

seven <- mclapply(321:350, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()
seven_read <- format(seven, scientific = FALSE)
eight <- mclapply(351:450, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()

eight_read <- format(eight, scientific = FALSE)

nine <- mclapply(451:500, Local_EONR_each_field,mc.cores = detectCores() - 1  ) %>%rbindlist()

nine_read <- format(nine, scientific = FALSE)

#mclapply(1:nrow(test_data_field_2), Brf_entire_data_field_2, mc.cores = detectCores() - 1)


results_five_hundred_fields <- rbind(fifty, Second_fifty_fiels, third_fifty_fields,forth_fifty_fields,fifth_fifty_plus_fields,
                                     twenty_after_three_hundred,seven,eight, nine )%>% format(., scientific = FALSE)

tr <- mclapply(1:500, f, mc.cores = detectCores() - 1  ) %>%rbindlist()

desi <-  filter(tr, method %in% c("CF_EONR", "CF_trained_on_entire_data", "Linear_EONR", "Linear_trained_on_entire_data"))


