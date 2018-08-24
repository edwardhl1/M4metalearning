#' Forecasting methods list
#' A list of the forecasting methods for use in the metalearnig process
#' The list follows the format described in the parameter \code{methods}
#' of \code{\link{calc_forecasts}}
#' @seealso \code{\link{calc_forecasts}}
#' @export
#' 

#devtools::install_github("edwardhl1/M4metalearning")
#devtools::install_github("robjhyndman/M4metalearning")
#library(M4metalearning)
#library(forecast)

forec_methods <- function() {
  methods_list <- list("auto_arima_forec")
  methods_list <- append(methods_list, "ets_forec")
  methods_list <- append(methods_list, "nnetar_forec")
  methods_list <- append(methods_list, "tbats_forec")
  methods_list <- append(methods_list, "stlm_ar_forec")
  methods_list <- append(methods_list, "rw_drift_forec")
  methods_list <- append(methods_list, "thetaf_forec")
  methods_list <- append(methods_list, "naive_forec")
  methods_list <- append(methods_list, "snaive_forec")
  methods_list
}

#' @describeIn forec_methods forecast::snaive
#' @param x A \code{ts} object with the input time series
#' @param h The amount of future time steps to forecast
#' @export
snaive_forec <- function(x, h, level = 55) {
  model <- forecast::snaive(x, h=h, level=level)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1]) #output list of mean forecast and high level estimate
  #frq <- stats::frequency(x) #maybe faster calculation
  #utils::tail(x,frq)[((1:h -1) %% frq) + 1]
}

#' @describeIn forec_methods forecast::naive
#' @export
naive_forec <- function(x, h, level = 55) {
  model <- forecast::naive(x, h=h, level = level)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}

#' @describeIn forec_methods forecast::auto.arima
#' @export
auto_arima_forec <- function(x, h, level = 55) {
  model <- forecast::auto.arima(x, stepwise=FALSE, approximation=FALSE)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}

#' @describeIn forec_methods forecast::ets
#' @export
ets_forec <- function(x, h, level = 55) {
  model <- forecast::ets(x)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}

#' @describeIn forec_methods forecast::nnetar
#' @export
nnetar_forec <- function(x, h, level = 55) {
  model <- forecast::nnetar(x)
  fore <- forecast::forecast(model, h=h)$mean
  list(fore, c(rep(NA,h))) #Create empty list of high level estimates to avoid high computation time of nnetar
}

#' @describeIn forec_methods forecast::tbats
#' @export
tbats_forec <- function(x, h, level = 55) {
  model <- forecast::tbats(x, use.parallel=FALSE)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}


#' @describeIn forec_methods forecast::stlm with ar modelfunction
#' @export
stlm_ar_forec <- function(x, h, level = 55) {
  model <- tryCatch({
    forecast::stlm(x, modelfunction = stats::ar)
  }, error = function(e) forecast::auto.arima(x, d=0,D=0))
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}

#' @describeIn forec_methods forecast::rwf
#' @export
rw_drift_forec <- function(x, h, level = 55) {
  model <- forecast::rwf(x, drift=TRUE, h=h, level=level)
  fore <- forecast::forecast(model, h=h, level=level)
  list(fore$mean, fore$upper[,1])
}


#' @describeIn forec_methods forecast::thetaf
#' @export
thetaf_forec <- function(x, h, level = 55) {
  fore <- forecast::thetaf(x, h=h, level = level)
  list(fore$mean, fore$upper[,1])
}

