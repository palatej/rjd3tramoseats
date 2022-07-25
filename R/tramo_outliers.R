#' @include utils.R
#' @importFrom stats is.ts frequency
NULL

#' Detect Outliers in TRAMO Model
#'
#' @param y the dependent variable (`ts` object).
#' @param order,seasonal the orders of the ARIMA model.
#' @param mean boolean to include or not the mean.
#' @param X explanatory varibales.
#' @param X.td trading days regressors.
#' @param ao,ls,so,tc boolean to indicate which outliers are detected
#' @param cv  `numeric`. The entered critical value for the outliers' detection procedure.
#' If equal to 0 the critical value for the outliers' detection procedure is automatically determined
#' by the number of observations.
#' @param ml Use of maximum likelihood (otherwise approximation by means of Hannan-Rissanen).
#'
#' @return a `"JDSTS"` object.
#'
#' @examples
#' tramo.outliers(rjd3toolkit::ABS$X0.2.09.10.M)
#' @export
tramo.outliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                      X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0, ml=F){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    td<-rjd3modelling::td(s = y, groups=X.td)
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
