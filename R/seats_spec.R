
#' Set Seats Specification
#'
#' @param x the specification.
#'
#' @param approximation character: the approximation mode. When the ARIMA model estimated by TRAMO does not accept an admissible decomposition, SEATS: `"None"` - performs an approximation; `"Legacy"` - replaces the model with a decomposable one; `"Noisy"` - estimates a new model by adding a white noise to the non-admissible model estimated by TRAMO.
#' @param trend.boundary numeric: the trend boundary. The boundary beyond which an AR root is integrated in the trend component.
#' If the modulus of the inverse real root is greater than the trend boundary, the AR root is integrated in the trend component.
#' Below this value, the root is integrated in the transitory component.
#' @param seas.boundary numeric: the seasonal boundary. The boundary beyond which a negative AR root is integrated in the seasonal component.
#' @param seas.boundary.unique numeric: the seasonal boundary (unique). The boundary beyond which a negative AR root is integrated
#' in the seasonal component, when the root is the unique seasonal root.
#' @param seas.tolerance numeric: the seasonal tolerance. The tolerance (measured in degrees) to allocate the AR non-real roots
#' to the seasonal component (if the modulus of the inverse complex AR root is greater than the trend boundary
#' and the frequency of this root differs from one of the seasonal frequencies by less than Seasonal tolerance)
#' or the transitory component (otherwise).
#' @param ma.boundary numeric: the MA unit root boundary. When the modulus of an estimated MA root falls in the range (xl, 1),
#' it is set to xl.
#' @param algorithm character: the estimation method for the unobserved components. The choice can be made from:
#'
#' - `"Burman"`: the default value. May result in a significant underestimation of the components' standard deviation,
#' as it may become numerically unstable when some roots of the MA polynomial are near 1;
#' - `"KalmanSmoother"`: it is not disturbed by the (quasi-) unit roots in MA;
#' - `"McElroyMatrix"`: it has the same stability issues as the Burman's algorithm.
#' @param bcasts,fcasts numeric: the number of backasts (`bcasts`) or forecasts (`fcasts`) used in the decomposition in periods (positive values) or years (negative values).
#' @param bias TODO.
#' @export
set_seats <- function(x,
                      approximation = c(NA, "None", "Legacy", "Noisy"),
                      trend.boundary = NA,
                      seas.boundary = NA,
                      seas.boundary.unique = NA,
                      seas.tolerance = NA,
                      ma.boundary = NA,
                      fcasts = NA,
                      bcasts = NA,
                      algorithm = c(NA, "Burman", "KalmanSmoother", "McElroyMatrix"),
                      bias = NA){
  UseMethod("set_seats", x)
}
#' @export
set_seats.JD3_SEATS_SPEC <- function(x,
                               approximation = c(NA, "None", "Legacy", "Noisy"),
                               trend.boundary = NA,
                               seas.boundary = NA,
                               seas.boundary.unique = NA,
                               seas.tolerance = NA,
                               ma.boundary = NA,
                               fcasts = NA,
                               bcasts = NA,
                               algorithm = c(NA, "Burman", "KalmanSmoother", "McElroyMatrix"),
                               bias = NA) {

  approximation <- match.arg(toupper(approximation[1]),
                             c(NA, "NONE", "LEGACY", "NOISY"))
  algorithm <- match.arg(toupper(algorithm[1]),
                         c(NA, "BURMAN", "KALMANSMOOTHER", "MCELROYMATRIX"))
  if (!is.na(approximation)) {
    x$approximation <- sprintf("APP_%s", approximation)
  }
  if (!is.na(algorithm)) {
    x$algorithm <- sprintf("ALG_%s", algorithm)
  }
  if (!is.na(seas.tolerance)) {
    x$epsphi <- seas.tolerance
  }
  if (!is.na(trend.boundary)) {
    x$rmod <- trend.boundary
  }
  if (!is.na(seas.boundary)) {
    x$sbound <- seas.boundary
  }
  if (!is.na(seas.boundary.unique)) {
    x$sboundatpi <- seas.boundary.unique
  }
  if (!is.na(ma.boundary)) {
    x$xl <- ma.boundary
  }


  if (!is.na(bcasts)) {
    x$nbcasts <- bcasts
  }
  if (!is.na(fcasts)) {
    x$nfcasts <- fcasts
  }
  if (!is.na(bias) && is.logical(bias)) {
    x$bias <- bias
  }
  x
}

#' @export
set_seats.JD3_TRAMOSEATS_SPEC <- function(x, ...) {
  x$seats <- set_seats(x$seats, ...)
  x
}
