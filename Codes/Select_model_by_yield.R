read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data field #2
data_field_1 <- read_data[3, ]$reg_data[[1]]$data[[1]]
#data <- data[3, ]$reg_data[[1]]$data[[2]]

data_to_work_with_field_1 <- data_field_1[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt")]
# %>% tibble::rowid_to_column(., "id")

data_sf_2_field_1 <- st_as_sf(data_to_work_with_field_1, coords = c("X", "Y"))

################################################################################
#RF field_1

RF_train <- 
  ranger(
    yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
    data = data_to_work_with_field_1
  )

#RF function to predict EONR for each observation on the entire data

rf_yield_based_selection <- function(i){ 
  row=1
  times = 134
  
  # random forest predict 
  #prediction for each cell
  data_cell <- data_to_work_with[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(RF_train, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

RF_yield_based <- mclapply(1:nrow(data_to_work_with), rf_yield_based_selection, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_RF <- sum(RF_yield_based$.)   #40508604151

################################################################################
#BRF_train  

train_data <- data_to_work_with%>% subset(., select=-c(X,Y))

test_data <- data_to_work_with%>% subset(., select=-c(X,Y,yield))



x <-as.matrix(train_data)%>%subset(., select=-c(yield))   
y <- train_data$yield

boosted.forest_train <- boosted_regression_forest(x, y)

#BRF function to predict EONR for each observation on the entire data

brf_yield_based_selection <- function(i){ 
  row=1
  times = 134
  
  # BRF predict 
  #prediction for each cell
  data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(boosted.forest_train, data_cell)$predictions
  
  data_cell$y_hat <- pred
  data_cell$yield <- data_to_work_with[i,13]
  
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

BRF_yield_based <- mclapply(1:nrow(data_to_work_with), brf_yield_based_selection, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_BRF <- sum(BRF_yield_based$.)    1.053223e+12

################################################################################
# linear regression  train   
l_train_data <- data_to_work_with

l_test_data <- data_to_work_with

# linear regression  train   

linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
               theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
               plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
             data = l_train_data)


linear_yield_based_selection <- function(i){ 
  
  # linear predict 
  
  row=1
  times = 134
  #prediction for each cell
  test_data_cell <- l_test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  predict_linear <-  linear %>% predict(test_data_cell)
  
  
  test_data_cell$y_hat <-  predict_linear
  test_data_cell <-  test_data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(test_data_cell$y_mines_y_hat)
  return(MSE)
}

linear_yield_based <- mclapply(1:1440, linear_yield_based_selection, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_linear <- sum(linear_yield_based$.)  #2.55373e+11

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#field_3

#RF

RF_train_yb_field_3 <- 
  ranger(
    yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
    data = data_to_work_with_field_3
  )

#RF function to predict EONR for each observation on the entire data

rf_yield_based_selection_field_3 <- function(i){ 
  row=1
  times = 134
  
  # random forest predict 
  #prediction for each cell
  data_cell <- data_to_work_with_field_3[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(RF_train_yb_field_3, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

RF_yield_based_field_3 <- mclapply(1:nrow(data_to_work_with_field_3), rf_yield_based_selection_field_3, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_RF_field_3 <- sum(RF_yield_based_field_3$.)   

################################################################################
#BRF_train  

train_data_yb_brf <- data_to_work_with_field_3%>% subset(., select=-c(X,Y))

test_data_yb_brf <- data_to_work_with_field_3%>% subset(., select=-c(X,Y,yield))



x_yb_brf <-as.matrix(train_data_yb_brf)%>%subset(., select=-c(yield))    
y_yb_brf <- train_data_yb_brf$yield

boosted.forest_train_yb_field_3 <- boosted_regression_forest(x_yb_brf, y_yb_brf)

#BRF function to predict EONR for each observation on the entire data

brf_yield_based_selection_field_3 <- function(i){ 
  row=1
  times = 134
  
  # BRF predict 
  #prediction for each cell
  data_cell <- test_data_yb_brf[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(boosted.forest_train_yb_field_3, data_cell)$predictions
  
  data_cell$y_hat <- pred
  data_cell$yield <- data_to_work_with_field_3[i,13]
  
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

BRF_yield_based_field_3 <- mclapply(1:nrow(data_to_work_with_field_3), brf_yield_based_selection_field_3, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_BRF_field_3 <- sum(BRF_yield_based_field_3$.)   

################################################################################
# linear regression  train   
l_train_data_field_3 <- data_to_work_with_field_3

l_test_data_field_3 <- data_to_work_with_field_3

# linear regression  train   

linear_yb_field_3 <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
               theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
               plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
             data = l_train_data_field_3)


linear_yield_based_selection_field_3 <- function(i){ 
  
  # linear predict 
  
  row=1
  times = 134
  #prediction for each cell
  test_data_cell <- l_test_data_field_3[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  predict_linear <-  linear_yb_field_3 %>% predict(test_data_cell)
  
  
  test_data_cell$y_hat <-  predict_linear
  test_data_cell <-  test_data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(test_data_cell$y_mines_y_hat)
  return(MSE)
}

linear_yield_based_field_3 <- mclapply(1:1440, linear_yield_based_selection_field_3, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_linear_field_3 <- sum(linear_yield_based_field_3$.) 







################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#field_2

#RF
RF_train_yb_field_2 <- 
  ranger(
    yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
    data = data_to_work_with_field_2
  )

#RF function to predict EONR for each observation on the entire data

rf_yield_based_selection_field_2 <- function(i){ 
  row=1
  times = 134
  
  # random forest predict 
  #prediction for each cell
  data_cell <- data_to_work_with_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(RF_train_yb_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

RF_yield_based_field_2 <- mclapply(1:nrow(data_to_work_with_field_2), rf_yield_based_selection_field_2, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_RF_field_2 <- sum(RF_yield_based_field_2$.)   

################################################################################
#BRF_train  

train_data_yb_brf_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y))

test_data_yb_brf_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y,yield))



x_yb_brf_field_2 <-as.matrix(train_data_yb_brf_field_2)%>%subset(., select=-c(yield))    
y_yb_brf_field_2 <- train_data_yb_brf_field_2$yield

boosted.forest_train_yb_field_2 <- boosted_regression_forest(x_yb_brf_field_2, y_yb_brf_field_2)

#BRF function to predict EONR for each observation on the entire data

brf_yield_based_selection_field_2 <- function(i){ 
  row=1
  times = 134
  
  # BRF predict 
  #prediction for each cell
  data_cell <- test_data_yb_brf_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(boosted.forest_train_yb_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  data_cell$yield <- data_to_work_with_field_2[i,13]
  
  
  data_cell <- data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(data_cell$y_mines_y_hat)
  return(MSE)
  
}

BRF_yield_based_field_2 <- mclapply(1:nrow(data_to_work_with_field_2), brf_yield_based_selection_field_2, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_BRF_field_2 <- sum(BRF_yield_based_field_2$.)    

################################################################################
# linear regression  train   
l_train_data_field_2 <- data_to_work_with_field_2

l_test_data_field_2 <- data_to_work_with_field_2

# linear regression  train   

linear_yb_field_2 <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                          theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                          plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
                        data = l_train_data_field_2)


linear_yield_based_selection_field_2 <- function(i){ 
  
  # linear predict 
  
  row=1
  times = 134
  #prediction for each cell
  test_data_cell <- l_test_data_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  predict_linear <-  linear_yb_field_2 %>% predict(test_data_cell)
  
  
  test_data_cell$y_hat <-  predict_linear
  test_data_cell <-  test_data_cell%>% mutate(y_mines_y_hat=(yield-y_hat)^2)
  
  
  MSE <- sum(test_data_cell$y_mines_y_hat)
  return(MSE)
}

linear_yield_based_field_2 <- mclapply(1:1440, linear_yield_based_selection_field_2, mc.cores = detectCores() - 1)%>%
  unlist() %>% data_frame()

MSE_yield_based_linear_field_2 <- sum(linear_yield_based_field_2$.) 

