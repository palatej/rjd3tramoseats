#' @importFrom rjd3toolkit .JD3_ENV

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
regarima_rslts<-NULL
p2r_sa_diagnostics<-NULL
p2r_sa_decomposition<-NULL


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
  regarima_rslts<<-.JD3_ENV$regarima_rslts
  DATE_MIN<<-.JD3_ENV$DATE_MIN
  DATE_MAX<<-.JD3_ENV$DATE_MAX
}

