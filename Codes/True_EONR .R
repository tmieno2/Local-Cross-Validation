
read_data <- readRDS("Shared/Data/all_sim_data.rds")

data_field_2 <- read_data[3, ]$reg_data[[1]]$data[[2]]

data_for_obtaining_true_field_2 <-
  data_for_obtaining_true_field_2 %>%
  .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
  .[, opt_N := pmin(Nk, opt_N)] %>%
  .[, opt_N := pmax(0, opt_N)]
