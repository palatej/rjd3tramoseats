#' @include utils.R
NULL


#' Title
#'
#' @param sarima
#' @param eps
#' @param rmod
#' @param smod
#' @param smodatpi
#' @param approximation
#'
#' @return
#' @export
#'
#' @examples
seats.decompose<-function(sarima, eps=2, rmod=.5, smod=.8,
                          smodatpi=.8, approximation=c("None", "Legacy", "Noisy")){
  if (!inherits(sarima, "JD3_SARIMA"))
    stop("Invalid model")
  approximation<-match.arg(approximation)
  jsarima<-rjd3modelling:::r2jd_sarima(sarima)
  jucm<-.jcall("demetra/tramoseats/r/Seats", "Ljdplus/ucarima/UcarimaModel;", "decompose",
         jsarima, eps, rmod, smod, smodatpi, approximation)
  if (is.jnull(jucm)) return (NULL) else return (rjd3modelling:::jd2r_ucarima(jucm))
}
