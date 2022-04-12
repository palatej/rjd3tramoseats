#' @include utils.R
#' @importFrom stats is.ts frequency
NULL

#' Title
#'
#' @param y
#' @param order
#' @param seasonal
#' @param mean
#' @param X
#' @param X.td
#' @param ao
#' @param ls
#' @param so
#' @param tc
#' @param cv
#' @param ml
#'
#' @return
#' @export
#'
#' @examples
tramo.outliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                      X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0, ml=F){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    td<-rjd3modelling::td.forTs(y, groups=X.td)
    X<-cbind(X, td)
  }


  jtramo<-.jcall("demetra/tramoseats/r/TramoOutliersDetection", "Ldemetra/tramoseats/r/TramoOutliersDetection$Results;", "process", rjd3toolkit::ts_r2jd(y),
               as.integer(order), as.integer(seasonal), mean, rjd3toolkit::matrix_r2jd(X),
               ao, ls, tc, so, cv, ml)
  model<-list(
    y=as.numeric(y),
    variables=rjd3toolkit::proc_vector(jtramo, "variables"),
    X=rjd3toolkit::proc_matrix(jtramo, "regressors"),
    b=rjd3toolkit::proc_vector(jtramo, "b"),
    bcov=rjd3toolkit::proc_matrix(jtramo, "bvar"),
    linearized=rjd3toolkit::proc_vector(jtramo, "linearized")
  )

  ll0<-rjd3toolkit::proc_likelihood(jtramo, "initiallikelihood.")
  ll1<-rjd3toolkit::proc_likelihood(jtramo, "finallikelihood.")

  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
