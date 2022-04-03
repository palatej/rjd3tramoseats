#' @importFrom rjd3arima add_outlier
#' @export
add_outlier.JD3_TRAMOSEATS_SPEC <- function(x,
                                     ...){
  x$regarima <- add_outlier(x$regarima,
                            ...)
  x
}
#' @importFrom rjd3arima add_ramp
#' @export
add_ramp.JD3_TRAMOSEATS_SPEC <- function(x,
                                  ...){
  x$regarima <- add_ramp(x$regarima,
                         ...)
  x
}
#' @importFrom rjd3arima set_arima
#' @export
set_arima.JD3_TRAMOSEATS_SPEC <- function(x,
                                   ...){
  x$regarima <- set_arima(x$regarima,
                          ...)
  x
}
#' @importFrom rjd3arima set_automodel
#' @export
set_automodel.JD3_TRAMOSEATS_SPEC <- function(x,
                                       ...){
  x$regarima <- set_automodel(x$regarima,
                              ...)
  x
}
#' @importFrom rjd3arima set_easter
#' @export
set_easter.JD3_TRAMOSEATS_SPEC <- function(x,
                                    ...){
  x$regarima <- set_easter(x$regarima,
                           ...)
  x
}
#' @importFrom rjd3arima set_estimate
#' @export
set_estimate.JD3_TRAMOSEATS_SPEC <- function(x,
                                      ...){
  x$regarima <- set_estimate(x$regarima,
                             ...)
  x
}
#' @importFrom rjd3arima set_outlier
#' @export
set_outlier.JD3_TRAMOSEATS_SPEC <- function(x,
                                     ...){
  x$regarima <- set_outlier(x$regarima,
                            ...)
  x
}
#' @importFrom rjd3arima set_tradingdays
#' @export
set_tradingdays.JD3_TRAMOSEATS_SPEC <- function(x,
                                         ...){
  x$regarima <- set_tradingdays(x$regarima,
                                ...)
  x
}
#' @importFrom rjd3arima set_transform
#' @export
set_transform.JD3_TRAMOSEATS_SPEC <- function(x,
                                       ...){
  x$regarima <- set_transform(x$regarima,
                              ...)
  x
}
#' @importFrom rjd3arima set_benchmarking
#' @export
set_benchmarking.JD3_TRAMOSEATS_SPEC <- function(x, ...) {
  x$benchmarking <- set_benchmarking(x$benchmarking, ...)

  x
}
