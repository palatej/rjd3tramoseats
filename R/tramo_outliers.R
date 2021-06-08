
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


  jtramo<-.jcall("demetra/tramoseats/r/TramoOutliersDetection", "Ldemetra/tramoseats/r/TramoOutliersDetection$Results;", "process", ts_r2jd(y),
               as.integer(order), as.integer(seasonal), mean, matrix_r2jd(X),
               ao, ls, tc, so, cv, ml)
  model<-list(
    y=as.numeric(y),
    variables=.JD3_ENV$proc_vector(jtramo, "variables"),
    X=.JD3_ENV$proc_matrix(jtramo, "regressors"),
    b=.JD3_ENV$proc_vector(jtramo, "b"),
    bcov=.JD3_ENV$proc_matrix(jtramo, "bvar"),
    linearized=.JD3_ENV$proc_vector(jtramo, "linearized")
  )

  ll0<-.JD3_ENV$proc_likelihood(jtramo, "initiallikelihood.")
  ll1<-.JD3_ENV$proc_likelihood(jtramo, "finallikelihood.")

  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
