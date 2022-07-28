#' @importFrom rjd3arima add_outlier
#' @export
add_outlier.JD3_TRAMOSEATS_SPEC <- function(x,
                                     ...){
  x$tramo <- add_outlier(x$tramo,
                            ...)
  x
}
#' @importFrom rjd3arima remove_outlier
#' @export
remove_outlier.JD3_TRAMOSEATS_SPEC <- function(x,
                                            ...){
  x$tramo <- remove_outlier(x$tramo,
                         ...)
  x
}
#' @importFrom rjd3arima add_ramp
#' @export
add_ramp.JD3_TRAMOSEATS_SPEC <- function(x,
                                  ...){
  x$tramo <- add_ramp(x$tramo,
                         ...)
  x
}
#' @importFrom rjd3arima remove_ramp
#' @export
remove_ramp.JD3_TRAMOSEATS_SPEC <- function(x,
                                         ...){
  x$tramo <- remove_ramp(x$tramo,
                      ...)
  x
}
#' @importFrom rjd3arima set_arima
#' @export
set_arima.JD3_TRAMOSEATS_SPEC <- function(x,
                                   ...){
  x$tramo <- set_arima(x$tramo,
                          ...)
  x
}
#' @importFrom rjd3arima set_automodel
#' @export
set_automodel.JD3_TRAMOSEATS_SPEC <- function(x,
                                       ...){
  x$tramo <- set_automodel(x$tramo,
                              ...)
  x
}
#' @importFrom rjd3arima set_easter
#' @export
set_easter.JD3_TRAMOSEATS_SPEC <- function(x,
                                    ...){
  x$tramo <- set_easter(x$tramo,
                           ...)
  x
}
#' @importFrom rjd3arima set_estimate
#' @export
set_estimate.JD3_TRAMOSEATS_SPEC <- function(x,
                                      ...){
  x$tramo <- set_estimate(x$tramo,
                             ...)
  x
}
#' @importFrom rjd3arima set_outlier
#' @export
set_outlier.JD3_TRAMOSEATS_SPEC <- function(x,
                                     ...){
  x$tramo <- set_outlier(x$tramo,
                            ...)
  x
}
#' @importFrom rjd3arima set_tradingdays
#' @export
set_tradingdays.JD3_TRAMOSEATS_SPEC <- function(x,
                                         ...){
  x$tramo <- set_tradingdays(x$tramo,
                                ...)
  x
}
#' @importFrom rjd3arima set_transform
#' @export
set_transform.JD3_TRAMOSEATS_SPEC <- function(x,
                                       ...){
  x$tramo <- set_transform(x$tramo,
                              ...)
  x
}
#' @importFrom rjd3arima set_benchmarking
#' @export
set_benchmarking.JD3_TRAMOSEATS_SPEC <- function(x, ...) {
  x$benchmarking <- set_benchmarking(x$benchmarking, ...)

  x
}
