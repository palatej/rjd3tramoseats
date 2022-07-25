#' @include utils.R
NULL


#' SEATS Decomposition
#'
#' @param sarima SARIMA model (see [rjd3modelling::sarima.model()]).
#' @inheritParams set_seats
#'
#'
#' @examples
#' seats.decompose(rjd3modelling::sarima.model(period = 12,phi = c(0,1),bd = 1))
#' @export
seats.decompose<-function(sarima, seas.tolerance=2, trend.boundary=.5, seas.boundary=.8,
                          seas.boundary.unique=.8, approximation=c("None", "Legacy", "Noisy")){
  if (!inherits(sarima, "JD3_SARIMA"))
    stop("Invalid model")
  approximation<-match.arg(approximation)
  jsarima<-rjd3modelling::r2jd_sarima(sarima)
  jucm<-.jcall("demetra/tramoseats/r/Seats", "Ljdplus/ucarima/UcarimaModel;", "decompose",
         jsarima, seas.tolerance, trend.boundary, seas.boundary, seas.boundary.unique, approximation)
  if (is.jnull(jucm)){
    return (NULL)
  } else {
    return (rjd3modelling::jd2r_ucarima(jucm))
  }
}
