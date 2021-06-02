#' @include utils.R jd3_r.R
NULL


p2r_parameters_rslt<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  return (data.frame(value=value, type=type))
}

p2r_parameters_rsltx<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  description<-sapply(p, function(z){z$description})

  rslt<-data.frame(value=value, type=type)
  row.names(rslt)<-description

  return (rslt)
}


p2r_parameters_estimation<-function(p){
  if (is.null(p))
    return (NULL)
  return (list(val=p$value, score=p$score, cov=p2r_matrix(p$covariance), description=p$description))
}


p2r_sarima<-function(p){
  return (list(name=p$name, period=p$period, phi = p$phi, d=p$d, theta=p$theta,
               bphi = p$bphi, bd = p$bd, btheta = p$btheta))
}

p2r_arima<-function(p){
  return (structure(list(name=p$name, innovationvariance=p$innovation_variance, ar=p$ar, delta=p$delta, ma=p$ma), class= "JD3ARIMA"))
}

ts_move<-function(period, freq, delta){
  if (delta == 0)return (period)
  if (freq == 1)return (c(period[1]+delta, 1))
  x<-period[1]*freq+(period[2]+delta-1)
  return (c(x %/% freq, (x %% freq)+1))
}


p2r_ucarima<-function(p){
  if (p$has("arima"))
    model<-p2r_arima(p$arima)
  else if (p$has("sarima"))
    model<-p2r_sarima(p$sarima)
  else
    model<-NULL
  return (structure
          (list(
            model=model,
            components=lapply(p$components, function(z){p2r_arima(z)})),
            class= "JD3UCARIMA"))
}

dateOf<-function(year, month, day){
  d<-jd3.Date$new()
  d$year<-year
  d$month<-month
  d$day<-day
  return (d)
}

parseDate<-function(s){
  d<-jd3.Date$new()
  d$year<-yearOf(s)
  d$month<-monthOf(s)
  d$day<-dayOf(s)
  return (d)
}


p2r_date<-function(p){
  if (p$has('year')){
    return (ymd(p$year, p$month, p$day))
  }else{
    return (NULL)
  }
}

r2p_date<-function(s){
  if (is.null(s)) return(jd3.Date$new())
  else return (parseDate(s))
}


# Span

p2r_span<-function(span){
  type<-enum_extract(jd3.SelectionType, span$type)
  dt0<-p2r_date(span$d0)
  dt1<-p2r_date(span$d1)

  return (structure(list(type=type, d0=dt0, d1=dt1, n0=span$n0, n1=span$n1), class= "JD3SPAN"))
}

r2p_span<-function(rspan){
  pspan<-jd3.TimeSelector$new()
  pspan$type<-enum_of(jd3.SelectionType, rspan$type, "SPAN")
  pspan$n0<-rspan$n0
  pspan$n1<-rspan$n1
  pspan$d0<-r2p_date(rspan$d0)
  pspan$d1<-r2p_date(rspan$d1)
  return (pspan)
}

# Parameter

# Matrix in the following form:
# row(1): values
# row(2): Parameters type

r2p_parameter<-function(r){
  p<-jd3.Parameter$new()
  if (is.null(r)) return (p)

  p$value<-r$value
  p$type<-enum_of(jd3.ParameterType, r$type, "PARAMETER")
  return (p)
}

p2r_parameter<-function(p){
  if (! p$has("type")) return (NULL)
  return (list(value = p$value, type=enum_extract(jd3.ParameterType, p$type)))
}

r2p_parameters<-function(r){

  n<-length(r)
  if (n == 0) return (NULL)
  p<-apply(r, 2, function(z){r2p_parameter(z)})
  return (p)
}

p2r_parameters<-function(p){
  n<-length(p)
  if (n == 0) return (NULL)
  r<-sapply(p, function(z){list(value=z$value, type=enum_extract(jd3.ParameterType, z$type))})
  return (r)
}

# Sarima

p2r_spec_sarima<-function(spec){
  return (list(
    period=spec$period,
    d=spec$d,
    bd=spec$bd,
    phi=p2r_parameters(spec$phi),
    theta=p2r_parameters(spec$theta),
    bphi=p2r_parameters(spec$bphi),
    btheta=p2r_parameters(spec$btheta)
  ))
}

r2p_spec_sarima<-function(r){
  p<-regarima.SarimaSpec$new()
  p$period<-r$period
  p$d<-r$d
  p$bd<-r$bd
  p$phi<-r2p_parameters(r$phi)
  p$theta<-r2p_parameters(r$theta)
  p$bphi<-r2p_parameters(r$bphi)
  p$btheta<-r2p_parameters(r$btheta)
  return (p)
}

p2r_outlier<-function(p){
  return (list(
    name=p$name,
    pos=p2r_date(p$position),
    code=p$code,
    coef=p2r_parameter(p$coefficient)
  ))
}

r2p_outlier<-function(r){
  p<-modelling.Outlier$new()
  p$name=r$name
  p$code<-r$code
  p$position<-r2p_date(r$pos)
  p$coefficient<-r2p_parameter(r$coef)
  return (p)
}

p2r_outliers<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_outlier(z)}))
}

r2p_outliers<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_outlier(z)}))
}

p2r_ramp<-function(p){
  return (list(
    name=p$name,
    start=p2r_date(p$start),
    end=p2r_date(p$end),
    coef=p2r_parameter(p$coefficient)
  ))
}

r2p_ramp<-function(r){
  p<-modelling.Ramp$new()
  p$name<-r$name
  p$start<-r2p_date(r$start)
  p$end<-r2p_date(r$end)
  p$coefficient<-r2p_parameter(r$coefficient)
  return (p)
}

p2r_ramps<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_ramp(z)}))
}

r2p_ramps<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_ramp(z)}))
}

rlags<-function(l0, l1){
  if (l0 == 0 && l1 == 0) {return (NULL)}
  if (l0 == l1){return (l0)}
  else {
    if (l1 < l0) stop("Invalid lags")
    return (c(l0, l1))
  }
}

regeffect<-function(map){
  r<-which(sapply(map, function(z){z$key == "regeffect"}))
  if (length(r) == 0) return ("Undefined")
  return (map[min(r)]$value)
}

p2r_uservar<-function(p){
  l0<-p$first_lag
  l1<-p$last_lag
  lapply
  return (list(
    id=p$id,
    name=p$name,
    lags=rlags(l0, l1),
    coef=p2r_parameter(p$coefficient),
    regeffect=regeffect(p$metadata)
  ))
}

r2p_uservar<-function(r){
  p<-modelling.TsVariable.$new()
  p$name<-r$name
  p$id<-r$id
  if (! is.null(r$lags)){
    if (length(r$lags) ==1){
      p$first_lag<-r$lags[1]
      p$last_lag<-r$lags[1]
    }else if (length(r$lags) ==2){
      p$first_lag<-r$lags[1]
      p$last_lag<-r$lags[2]
    }else
      stop("Invalid lags")
  }
  p$coefficient<-r2p_parameters(r$coef)
  p$metadata<-list(list(key="regeffect", value=r$regeffect))
  return (p)
}

p2r_uservars<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){p2r_uservar(z)}))
}

r2p_uservars<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){r2p_uservar(z)}))
}


# Benchmarking

p2r_spec_benchmarking<-function(p){
  return (list(
    enabled=p$enabled,
    target=enum_extract(sa.BenchmarkingTarget, p$target),
    lambda=p$lambda,
    rho=p$rho,
    bias=enum_extract(sa.BenchmarkingBias, p$bias),
    forecast=p$forecast
  ))
}

r2p_spec_benchmarking<-function(r){
  p<-sa.BenchmarkingSpec$new()
  p$enabled<-r$enabled
  p$target<-enum_of(sa.BenchmarkingTarget, r$target, "BENCH")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-enum_of(sa.BenchmarkingBias, r$bias, "BENCH")
  p$forecast<-r$forecast
  return (p)
}



# Benchmarking

p2r_spec_benchmarking<-function(p){
  return (list(
    enabled=p$enabled,
    target=enum_extract(sa.BenchmarkingTarget, p$target),
    lambda=p$lambda,
    rho=p$rho,
    bias=enum_extract(sa.BenchmarkingBias, p$bias),
    forecast=p$forecast
  ))
}

r2p_spec_benchmarking<-function(r){
  p<-sa.BenchmarkingSpec$new()
  p$enabled<-r$enabled
  p$target<-enum_of(sa.BenchmarkingTarget, r$target, "BENCH")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-enum_of(sa.BenchmarkingBias, r$bias, "BENCH")
  p$forecast<-r$forecast
  return (p)
}
