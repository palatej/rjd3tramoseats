#' @include utils.R
#' @importFrom rjd3sa sa.decomposition
NULL

regarima_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", jrslts)
  rq<-RProtoBuf::read(regarima.RegArimaModel, q)
  return (rjd3modelling:::p2r_regarima_rslts(rq))
}

tramoseats_rslts <- function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", jrslts)
  rq<-RProtoBuf::read(tramoseats.TramoSeatsResults, q)
  return (p2r_tramoseats_rslts(rq))
}

p2r_tramoseats_rslts<-function(p){

  return (structure(
    list(
      preprocessing=rjd3modelling:::p2r_regarima_rslts(p$preprocessing),
      decomposition=p2r_seats_rslts(p$decomposition),
      final=rjd3sa:::p2r_sa_decomposition(p$final),
      diagnostics=rjd3sa:::p2r_sa_diagnostics(p$diagnostics_sa)
    ),
    class= c("JD3_TRAMOSEATS_RSLTS", "JD3")))
}



p2r_seats_rslts<-function(p){
  return (structure(
    list(
      seatsmodel=rjd3modelling:::p2r_arima(p$seats_arima),
      canonicaldecomposition=rjd3modelling:::p2r_ucarima(p$canonical_decomposition),
      stochastics=rjd3sa:::p2r_sa_decomposition(p$stochastics, T)),
    class= "JD3_SEATS"))
}

############################# Generics

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sa.decomposition.JD3_TRAMOSEATS_RSLTS<-function(x){
  if (is.null(x)) return (NULL)
  return (rjd3sa::sadecomposition(x$final$series$data,
                                x$final$sa$data,
                                x$final$t$data,
                                x$final$s$data,
                                x$final$i$data,
                                x$preprocessing$description$log
                                ))

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sa.decomposition.JD3_TRAMOSEATS_OUTPUT<-function(x){
  return (rjd3sa::sadecomposition(x$result))
}

