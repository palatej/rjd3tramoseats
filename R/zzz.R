#' @importFrom rjd3toolkit .JD3_ENV
NULL

DATE_MIN<-NULL
DATE_MAX<-NULL
ts_r2jd<-NULL
tsdomain_r2jd<-NULL
ts_jd2r<-NULL
matrix_jd2r<-NULL
matrix_r2jd<-NULL
jd2r_test<-NULL
enum_extract<-NULL
enum_of<-NULL
p2r_ts<-NULL
p2r_matrix<-NULL
p2r_regarima_rslts<-NULL
p2r_sa_diagnostics<-NULL
p2r_sa_decomposition<-NULL
p2r_arima<-NULL
p2r_ucarima<-NULL
p2r_span<-NULL
r2p_span<-NULL
p2r_spec_sarima<-NULL
r2p_spec_sarima<-NULL
p2r_parameter<-NULL
r2p_parameter<-NULL
p2r_parameters<-NULL
r2p_parameters<-NULL
p2r_outliers<-NULL
r2p_outliers<-NULL
p2r_ramps<-NULL
r2p_ramps<-NULL
p2r_spec_benchmarking<-NULL
r2p_spec_benchmarking<-NULL


.onLoad <- function(libname, pkgname) {
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  enum_extract<<-.JD3_ENV$enum_extract
  enum_of<<-.JD3_ENV$enum_of
  jd2r_test<<-.JD3_ENV$jd2r_test
  matrix_jd2r<<-.JD3_ENV$matrix_jd2r
  matrix_r2jd<<-.JD3_ENV$matrix_r2jd
  ts_jd2r<<-.JD3_ENV$ts_jd2r
  ts_r2jd<<-.JD3_ENV$ts_r2jd
  tsdomain_r2jd<<-.JD3_ENV$tsdomain_r2jd

  p2r_regarima_rslts<<-.JD3_ENV$p2r_regarima_rslts
  p2r_ts<<-.JD3_ENV$p2r_ts
  p2r_matrix<<-.JD3_ENV$p2r_matrix
  p2r_sa_diagnostics<<-.JD3_ENV$p2r_sa_diagnostics
  p2r_sa_decomposition<<-.JD3_ENV$p2r_sa_decomposition
  p2r_arima<<-.JD3_ENV$p2r_arima
  p2r_ucarima<<-.JD3_ENV$p2r_ucarima
  p2r_span<<-.JD3_ENV$p2r_span
  r2p_span<<-.JD3_ENV$r2p_span
  p2r_spec_sarima<<-.JD3_ENV$p2r_spec_sarima
  r2p_spec_sarima<<-.JD3_ENV$r2p_spec_sarima
  p2r_parameter<<-.JD3_ENV$p2r_parameter
  r2p_parameter<<-.JD3_ENV$r2p_parameter
  p2r_parameters<<-.JD3_ENV$p2r_parameters
  r2p_parameters<<-.JD3_ENV$r2p_parameters
  p2r_outliers<<-.JD3_ENV$p2r_outliers
  r2p_outliers<<-.JD3_ENV$r2p_outliers
  p2r_ramps<<-.JD3_ENV$p2r_ramps
  r2p_ramps<<-.JD3_ENV$r2p_ramps
  p2r_spec_benchmarking<<-.JD3_ENV$p2r_spec_benchmarking
  r2p_spec_benchmarking<<-.JD3_ENV$r2p_spec_benchmarking
  DATE_MIN<<-.JD3_ENV$DATE_MIN
  DATE_MAX<<-.JD3_ENV$DATE_MAX

  ###
  .JD3_ENV$jd2r_spec_tramo<-jd2r_spec_tramo
  .JD3_ENV$r2jd_spec_tramo<-r2jd_spec_tramo
  .JD3_ENV$jd2r_spec_tramoseats<-jd2r_spec_tramoseats
  .JD3_ENV$r2jd_spec_tramoseats<-r2jd_spec_tramoseats
  .JD3_ENV$tramoseats_rslts<-tramoseats_rslts
  .JD3_ENV$tramo_rslts<-regarima_rslts

}

