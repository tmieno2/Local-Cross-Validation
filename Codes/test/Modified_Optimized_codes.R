---
  title: "Optimized_codes"
author: "Mona Mousavi"
date: "2023-01-10"
output: html_document
---
  
  # Read data
  ```{r}
library(here)
read_data <- readRDS(here("all_sim_data.rds"))
data_field_1 <- read_data[3, ]$reg_data[[1]]$data[[1]]
data <- data_field_1[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                         "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                         "plateau_1_2" ,"yield" ,"X", "Y" ,"aunit_id",  "N_tgt")] 


data_sf <- st_as_sf(data, coords = c("X", "Y"))

```


# Import corn and N price
```{r}
pCorn <- readRDS(here("pCorn.rds"))
pN <- readRDS(here("pN.rds"))
```

# Create folds in a spatially clustered manner
```{r}
gen_repeated_sptial_folds <- function(i) {
  skcv_folds_try <- spatial_clustering_cv(data_sf, v = 6)
  
  scv_try <-
    skcv_folds_try %>%
    rowwise() %>%
    mutate(
      training_data = list(analysis(splits) %>% data.table()),
      test_data = list(assessment(splits) %>% data.table())
    ) %>%
    mutate(repeats = i)
  
  return(scv_try)
}

spatial_folds <-
  lapply(1:10, gen_repeated_sptial_folds) %>%
  rbindlist()

```

# S-leaner (BRF)

```{r}
cov_list <- c(
  "N_tgt", "theta_b2_2", "theta_b2_1", "theta_b1_2", "theta_b1_1", "Nk_2_1", "Nk_2_2",
  "Nk_1_1", "Nk_1_2", "plateau_2_1", "plateau_2_2", "plateau_1_1", "plateau_1_2"
)

find_local_EONR_BRF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  boosted_forest <- boosted_regression_forest(X, Y)
  
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(boosted_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

```


# S-leaner (RF)

```{r}
find_local_EONR_RF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y)
  
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
}

```

# S-leaner (Linear)

```{r}
find_local_EONR_Linear <- function(data,test_data, train_data){
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  
  
  linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2), 
               data = train_data)
  
  

  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()
  
  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(linear, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]
  
  return(opt_EONR)
} 
```

# S-leaner (gam)

```{r}
find_local_EONR_gam <- function(data,test_data){
  
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  
  gam <- gam(yield ~ s(N_tgt, k=3), data = test_data)
  
  opt_EONR <-
    copy(N_data) %>%
    .[, y_hat := predict(gam, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ]]
  
  return(opt_EONR)
}  
```

# all models

```{r}
find_local_EONR_all <- function(i, spatial_folds, data) {
  temp_fold <- spatial_folds[i, ]
  
  train_data <- temp_fold$training_data[[1]]
  test_data <- temp_fold$test_data[[1]]
  
  X <- as.matrix(train_data[, cov_list, with = FALSE])
  Y <- train_data$yield
  
  
  #linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2), 
               #data = train_data)
  
  #gam <- gam(yield ~ s(N_tgt, k=3), data = test_data)
  
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
    find_local_EONR_Linear(data,test_data,train_data) %>%
    .[, .(N_tgt = mean(N_tgt))] %>%
    .[, method := "S-learner (Linear)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## GAM
  # /*+++++++++++++++++++++++++++++++++++
  opt_EONR_gam <-
    find_local_EONR_gam(data,test_data) %>%
    .[, .(N_tgt = N_tgt)] %>%
    .[, method := "S-learner (GAM)"]
  
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Combine
  # /*+++++++++++++++++++++++++++++++++++
  mean_eonr_data <-
    rbind(opt_EONR_BRF, opt_EONR_RF, opt_EONR_Linear, opt_EONR_gam) %>%
    .[, id := temp_fold$id] %>%
    .[, repeats := temp_fold$repeats]
  
  return(mean_eonr_data)
}

```

test <- lapply(1:2, function(x) find_local_EONR_all(x, spatial_folds, data)) %>%
  rbindlist()












