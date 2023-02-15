scale_data <- function(data, scale_data, back = FALSE) {
  scaled_data <-
    lapply(
      names(data),
      function(x) {
        scaler <- data.table(scale_data)[variable == x, scaler]

        if (back == FALSE) {
          if (length(scaler) == 1) { # scale
            data[, ..x] * scaler
          } else { # do not scale
            data[, ..x]
          }
        } else {
          if (length(scaler) == 1) { # scale
            data[, ..x] / scaler
          } else { # do not scale
            data[, ..x]
          }
        }
      }
    ) %>%
    reduce(cbind)

  return(scaled_data)
}

expand_grid_df <- function(data_1, data_2) {
  data_1_ex <-
    data_1[rep(1:nrow(data_1), each = nrow(data_2)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  data_2_ex <-
    data_2[rep(1:nrow(data_2), nrow(data_1)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  expanded_data <-
    data_1_ex[data_2_ex, on = "rowid"] %>%
    .[, rowid := NULL]

  if ("tbl" %in% class(data_1)) {
    expanded_data <- as_tibble(expanded_data)
  }

  if ("rowwise_df" %in% class(data_1)) {
    expanded_data <- rowwise(expanded_data)
  }

  return(expanded_data)
}

#' Generate distance-based weight matrix
#'
#' @param data (data.frame)
#' @param cutoff (numeric)
#' @returns A matrix of weights
#' @import data.table
#' @export
gen_weight_matrix <- function(data, cutoff = 50) {
  data_sf <-
    st_as_sf(data) %>%
    st_centroid()

  D <- st_distance(data_sf, data_sf) # distance mat
  W <- 1 / D^2 # inverse distance
  W[D > cutoff] <- 0 # cut off distance
  diag(W) <- 0
  W <- W / rowSums(W) # row-standardize
  Wls <- spdep::mat2listw(W) # "listw" object

  return(Wls)
}

model_picker <- function(model) {
  if (model == "brf") {
    fcn <- find_opt_vra_BRF
  }

  if (model == "rf") {
    fcn <- find_opt_vra_RF
  }

  if (model == "lm") {
    fcn <- find_opt_vra_Linear
  }

  if (model == "se") {
    fcn <- find_opt_vra_SE
  }

  if (model == "cf") {
    fcn <- find_opt_vra_CF_smooth
  }

  if (model == "gam") {
    fcn <- find_opt_ura_gam
  }

  return(fcn)
}
