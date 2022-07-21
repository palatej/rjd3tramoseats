#' @include utils.R tramoseats_spec.R tramoseats_rslts.R
NULL


#' TRAMO model, pre-adjustment in TRAMO-SEATS
#'
#' @param ts a univariate time series.
#' @param spec the model specification. Can be either the name of a predefined specification or a user-defined specification.
#' @param context the dictionnary of variables.
#' @param userdefined a vector containing the additional output variables.
#'
#' @return the `tramo()` function returns a list with the results (`"JD3_REGARIMA_RSLTS"` object), the estimation specification and the result specification, while `fast.tramo()` is a faster function that only returns the results.
#'
#' @examples
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' sp = spec_tramo_default("trfull")
#' sp = add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' fast.tramo(y, spec = sp)
#' sp = set_transform(
#'   set_tradingdays(
#'     set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' fast.tramo(y, spec = sp)
#' sp = set_outlier(sp, outliers.type = c("AO"))
#' fast.tramo(y, spec = sp)
#' @export
tramo<-function(ts, spec="trfull", context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ljdplus/tramo/TramoOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ljdplus/tramo/TramoOutput;", "fullProcess", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = tramo_output(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined))
  }
}

#' @export
#' @rdname tramo
fast.tramo<-function(ts, spec="trfull", context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = regarima_rslts(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}


tramo_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/Tramo", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoOutput, q)
  return (structure(list(
    result=rjd3modelling::p2r_regarima_rslts(p$result),
    estimation_spec=p2r_spec_tramo(p$estimation_spec),
    result_spec=p2r_spec_tramo(p$result_spec)
  ),
  class=c("JD3_TRAMO_OUTPUT", "JD3"))
  )
}

#' Seasonal Adjustment with  TRAMO-SEATS
#'
#' @inheritParams tramo
#'
#'
#' @examples
#' sp = spec_tramoseats_default("rsafull")
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' fast.tramoseats(y, spec = sp)
#' sp = add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' sp = set_transform(
#'   set_tradingdays(
#'     set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' fast.tramoseats(y, spec = sp)
#' @export
tramoseats<-function(ts, spec="rsafull", context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/io/protobuf/TramoSeatsOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/io/protobuf/TramoSeatsOutput;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = tramoseats_output(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined))
  }
}

#' @export
#' @rdname tramoseats
fast.tramoseats<-function(ts, spec="rsafull", context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ljdplus/tramoseats/TramoSeatsResults;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/TramoSeats", "Ljdplus/tramoseats/TramoSeatsResults;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = tramoseats_rslts(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}

tramoseats_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/tramoseats/r/TramoSeats", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoSeatsOutput, q)
  return (structure(list(
    result=p2r_tramoseats_rslts(p$result),
    estimation_spec=p2r_spec_tramoseats(p$estimation_spec),
    result_spec=p2r_spec_tramoseats(p$result_spec)
  ),
  class="JD3TRAMOSEATS_OUTPUT")
  )

}

#' Refresh Policy
#'
#' @param spec the current specification
#' @param refspec the reference specification (used to defined the set of models considered).
#' By default this is the `"TRFull"` or `"RSAFull"` specification.
#' @param policy the refresh policy.
#' @param period,start,end to specify the frozen domain when `policy` equals to `"Outliers"` or `"Outliers_StochasticComponent"`.
#'
#' @return a new specification.
#' @examples
#' y = rjd3toolkit::ABS$X0.2.08.10.M
#' y_anc = window(y,end = 2009)
#' mod_anc = tramo(y_anc, spec_tramo_default())
#' res_spec = mod_anc$result_spec
#' mod_anc
#' # ARIMA parameters fixed
#' fast.tramo(y,
#'               tramo.refresh(res_spec,
#'                                mod_anc$estimation_spec,
#'                                policy = "FixedParameters"))
#' # Outlier detection
#' fast.tramo(y,
#'               tramo.refresh(res_spec,
#'                                policy = "Outliers"))
#' @name refresh
#' @rdname refresh
#' @export
tramo.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_TRAMO_SPEC"))
    stop("Invalid specification type")
  jspec<-r2jd_spec_tramo(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/tramo/TramoSpec", "Ldemetra/tramo/TramoSpec;", "fromString", "trfull")

  }else{
    if (!inherits(refspec, "JD3_TRAMO_SPEC"))
      stop("Invalid specification type")
    jrefspec<-r2jd_spec_tramo(refspec)
  }
  jdom<-rjd3toolkit::jdomain(period, start, end)
  jnspec<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/tramo/TramoSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_tramo(jnspec))
}

#' @rdname refresh
#' @export
tramoseats.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_TRAMOSEATS_SPEC"))
    stop("Invalid specification type")
  jspec<-r2jd_spec_tramoseats(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/tramoseats/TramoSeatsSpec", "Ldemetra/tramoseats/TramoSeatsSpec;", "fromString", "rsafull")

  }else{
    if (!inherits(refspec, "JD3_TRAMOSEATS_SPEC"))
      stop("Invalid specification type")
    jrefspec<-r2jd_spec_tramoseats(refspec)
  }
  jdom<-rjd3toolkit::jdomain(period, start, end)
  jnspec<-.jcall("demetra/tramoseats/r/TramoSeats", "Ldemetra/tramoseats/TramoSeatsSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_tramoseats(jnspec))

}



terror_names<-c("actual", "forecast", "error", "rel. error", "raw", "fraw", "efraw")
forecast_names<-c("forecast", "error", "fraw", "efraw")

#' TERROR
#'
#' @inheritParams tramo
#' @param nback
#'
#' @return
#' @export
#'
#' @examples
terror<-function(ts, spec="trfull", nback=1, context=NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/Matrix;", "process", jts, spec, as.integer(nback))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/Matrix;", "process", jts, jspec, jcontext, as.integer(nback))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-rjd3toolkit::matrix_jd2r(jrslt)
    colnames(rslt)<-terror_names
    return (rslt)
  }
}

#' Forecasts with TRAMO
#'
#' @inheritParams tramo
#' @param nf the forecasting horizon (`numeric`). The forecast length is in periods (positive values) or years (negative values). By default, the program generates a one-year forecast (`nf = -1`).
#'
#' @export
#'
tramo.forecast<-function(ts, spec="trfull", nf=-1, context=NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (nf<0) nf<-frequency(ts)*(-nf)

  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/Matrix;", "forecast", jts, spec, as.integer(nf))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }else{
      # TODO
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/Matrix;", "forecast", jts, jspec, jcontext, as.integer(nf))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-rjd3toolkit::matrix_jd2r(jrslt)
    colnames(rslt)<-forecast_names
    return (rslt)
  }
}
