`%>%` <- magrittr::`%>%`

#' Get LST Model
#'
#' @param asset
#'
#' @return
#' @export
#'
get_lstm_model <- function(asset = "BTC-USD"){

  if(asset == "BTC-USD"){
    files <- system.file("btc_model", package = "augur")
    scalers <- system.file("btc_scalers.Rds", package = "augur")
    hf_model_path <- system.file("btc_model.h5", package = "augur")
    new_model <- load_model_hdf5("my_model.h5")
   model <- list(lstm = keras::load_model_tf("btc_model"),
                 scalers = readRDS(scalers))
   return(model)
  } else {
    stop("Only BTC available at this time")
  }

}

#' Get Prediction Data
#'
#' @param asset
#' @param prediction
#'
#' @return
#' @export
#'
get_prediction_data <- function(asset = "BTC-USD", prediction = 5){


  today <- Sys.Date()
  five_days_ago <- Sys.Date() - 5
  new_data <- tidyquant::tq_get(asset, get = "stock.prices", from = five_days_ago) %>%
    dplyr::filter(!is.na(open))

  if(nrow(new_data) != 5){
    new_data <- tidyquant::tq_get(asset, get = "stock.prices", from = five_days_ago - 1) %>%
      dplyr::filter(!is.na(open))
  }

  return(new_data)

}

#' Scale Prediction Data
#'
#' @param data
#' @param scalers
#'
#' @return
#' @export
#'
scale_prediction_data <- function(data, scalers){

  data <- dplyr::mutate(data, open = (open - scalers$min_open) /
                      (scalers$max_open -  scalers$min_open)) %>%
    dplyr::pull(open)

}

#' Create Prediction Array
#'
#' @param data
#' @param lag
#'
#' @return
#' @export
#'
create_prediction_array <- function(data, lag = 5){

  if("data.frame" %in% class(data)) stop("Data must be numeric vector")

  pred_array <- array(
    data = data,
    dim = c(
      1,
      lag,
      1
    )
  )

  return(pred_array)
}

#' Predict LSTM
#'
#' @param lstm_model
#' @param pred_array
#'
#' @return
#' @export
#'
predict_lstm <- function(pred_array, lstm_model){
  lstm_predictions <- lstm_model %>%
    predict(pred_array, batch_size = 1) %>%
    .[, , 1]

  return(lstm_predictions)
}

#' Denormalize
#'
#' @param x
#' @param min_val
#' @param max_val
#'
#' @return
#' @export
#'
descale_prediction_data <- function(x,scalers) {
  x*(scalers$max_open - scalers$min_open) + scalers$min_open
}

#
# #
#  library(magrittr)
# lstm <- keras::load_model_tf("~/Documents/GitHub/asset_nn/btc_model")
# pred <- get_prediction_data() %>%
#  scale_prediction_data(scalers) %>%
#  create_prediction_array() %>%
#   predict_lstm(lstm) %>%
#   descale_prediction_data(scalers)
