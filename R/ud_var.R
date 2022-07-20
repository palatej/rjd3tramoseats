add_ud_var <- function(x, jx, userdefined = NULL, out_class = NULL, result = FALSE){
  if (is.null(userdefined)) {
    x$user_defined = rjd3toolkit::user_defined(x, NULL)
  } else {
    if (result) {
      res = jx
    } else {
      if (is.null(out_class)) {
        res = jx$getResult()
      } else {
        res = .jcall(jx, out_class, "getResult")
      }
    }
    res = rjd3toolkit::jd3Object(res, result = TRUE)
    x$user_defined = rjd3toolkit::user_defined(res, userdefined = userdefined)
  }
  x
}


#' Retrieve the user-defined variable names
#'
#' Function to retrieve the names of the additional output variables that can be defined in `x13()`, `regarima()` and `x11()` functions.
#'
#'@param x a character
#'
#' @examples
#' userdefined_variables_tramoseats("tramoseats")
#' userdefined_variables_tramoseats("tramo")
#' @export
userdefined_variables_tramoseats <- function(x = c("TRAMO-SEATS","TRAMO")){
x <- match.arg(gsub("-", "", tolower(x)),
               choices = c("tramoseats", "tramo"))

# jts<-rjd3toolkit::ts_r2jd(rjd3toolkit::ABS$X0.2.09.10.M)
# jrslt<- rJava::.jcall("demetra/tramoseats/r/TramoSeats", "Ljdplus/tramoseats/TramoSeatsResults;",
#                        "process", jts, "rsafull")
# rjd3toolkit::dictionary(rjd3toolkit::jd3Object(jrslt, result = TRUE)) |>
#   dput()

sa_tramoseats = structure(c("mode", "seasonal", "y", "t", "t_f", "t_ef", "t_b",
                            "t_eb", "sa", "sa_f", "sa_ef", "sa_b", "sa_eb", "s", "s_f", "s_ef",
                            "s_b", "s_eb", "i", "i_f", "i_ef", "i_b", "i_eb", "decomposition.y_lin",
                            "decomposition.y_lin_f", "decomposition.y_lin_ef", "decomposition.y_lin_b",
                            "decomposition.y_lin_eb", "decomposition.t_lin", "decomposition.t_lin_e",
                            "decomposition.t_lin_f", "decomposition.t_lin_ef", "decomposition.t_lin_b",
                            "decomposition.t_lin_eb", "decomposition.sa_lin", "decomposition.sa_lin_e",
                            "decomposition.sa_lin_f", "decomposition.sa_lin_ef", "decomposition.sa_lin_b",
                            "decomposition.sa_lin_eb", "decomposition.s_lin", "decomposition.s_lin_e",
                            "decomposition.s_lin_f", "decomposition.s_lin_ef", "decomposition.s_lin_b",
                            "decomposition.s_lin_eb", "decomposition.i_lin", "decomposition.i_lin_e",
                            "decomposition.i_lin_f", "decomposition.i_lin_ef", "decomposition.i_lin_b",
                            "decomposition.i_lin_eb", "decomposition.y_cmp", "decomposition.y_cmp_f",
                            "decomposition.y_cmp_ef", "decomposition.y_cmp_b", "decomposition.y_cmp_eb",
                            "decomposition.t_cmp", "decomposition.t_cmp_f", "decomposition.t_cmp_ef",
                            "decomposition.t_cmp_b", "decomposition.t_cmp_eb", "decomposition.sa_cmp",
                            "decomposition.sa_cmp_f", "decomposition.sa_cmp_ef", "decomposition.sa_cmp_b",
                            "decomposition.sa_cmp_eb", "decomposition.s_cmp", "decomposition.s_cmp_f",
                            "decomposition.s_cmp_ef", "decomposition.s_cmp_b", "decomposition.s_cmp_eb",
                            "decomposition.i_cmp", "decomposition.i_cmp_f", "decomposition.i_cmp_ef",
                            "decomposition.i_cmp_b", "decomposition.i_cmp_eb", "decomposition.initialmodel.p",
                            "decomposition.initialmodel.d", "decomposition.initialmodel.q",
                            "decomposition.initialmodel.bp", "decomposition.initialmodel.bd",
                            "decomposition.initialmodel.bq", "decomposition.initialmodel.phi(*)",
                            "decomposition.initialmodel.bphi(*)", "decomposition.initialmodel.theta(*)",
                            "decomposition.initialmodel.btheta(*)", "decomposition.finalmodel.p",
                            "decomposition.finalmodel.d", "decomposition.finalmodel.q", "decomposition.finalmodel.bp",
                            "decomposition.finalmodel.bd", "decomposition.finalmodel.bq",
                            "decomposition.finalmodel.phi(*)", "decomposition.finalmodel.bphi(*)",
                            "decomposition.finalmodel.theta(*)", "decomposition.finalmodel.btheta(*)",
                            "arima.phi(*)", "arima.bphi(*)", "arima.theta(*)", "arima.btheta(*)",
                            "y_f(?)", "y_b(?)", "y_ef(?)", "y_eb(?)", "yc", "l", "ycal",
                            "det", "det_f(?)", "det_b(?)", "cal", "cal_f(?)", "cal_b(?)",
                            "tde", "tde_f(?)", "tde_b(?)", "mhe", "mhe_f(?)", "mhe_b(?)",
                            "ee", "ee_f(?)", "ee_b(?)", "omhe", "omhe_f(?)", "omhe_b(?)",
                            "out", "out_f(?)", "out_b(?)", "regression.mu", "regression.lp",
                            "regression.easter", "regression.outlier(*)", "regression.td(*)",
                            "regression.user(*)", "regression.out(*)", "regression.missing(*)",
                            "regression.ml.pcorr", "residuals.ser", "residuals.res", "residuals.tsres",
                            "arima.p", "arima.d", "arima.q", "arima.bp", "arima.bd", "arima.bq",
                            "period", "span.start", "span.end", "span.n", "span.missing",
                            "log", "adjust", "regression.espan.start", "regression.espan.end",
                            "regression.espan.n", "regression.espan.missing", "regression.mean",
                            "regression.nlp", "regression.ntd", "regression.nmh", "regression.nout",
                            "regression.nao", "regression.nls", "regression.ntc", "regression.nso",
                            "regression.nusers", "regression.leaster", "regression.description",
                            "regression.type", "regression.details.coefficients", "regression.details.covar",
                            "regression.details.covar-ml", "regression.ml.parameters", "regression.ml.pcovar",
                            "regression.ml.pcovar-ml", "regression.ml.pscore", "likelihood.ll",
                            "likelihood.adjustedll", "likelihood.ssqerr", "likelihood.nparams",
                            "likelihood.nobs", "likelihood.neffectiveobs", "likelihood.df",
                            "likelihood.aic", "likelihood.aicc", "likelihood.bic", "likelihood.bicc",
                            "likelihood.bic2", "likelihood.hannanquinn", "residuals.type",
                            "residuals.mean", "residuals.doornikhansen", "residuals.skewness",
                            "residuals.kurtosis", "residuals.lb", "residuals.bp", "residuals.seaslb",
                            "residuals.seasbp", "residuals.lb2", "residuals.bp2", "residuals.nruns",
                            "residuals.lruns", "residuals.nudruns", "residuals.ludruns",
                            "out_i", "out_i_f(?)", "out_i_b(?)", "out_t", "out_t_f(?)", "out_t_b(?)",
                            "out_s", "out_s_f(?)", "out_s_b(?)", "reg_i", "reg_i_f(?)", "reg_i_b(?)",
                            "reg_t", "reg_t_f(?)", "reg_t_b(?)", "reg_s", "reg_s_f(?)", "reg_s_b(?)",
                            "reg_sa", "reg_sa_f(?)", "reg_sa_b(?)", "reg_y", "reg_y_f(?)",
                            "reg_y_b(?)", "reg_u", "reg_u_f(?)", "reg_u_b(?)", "det_i", "det_i_f(?)",
                            "det_i_b(?)", "det_t", "det_t_f(?)", "det_t_b(?)", "det_s", "det_s_f(?)",
                            "det_s_b(?)", "diagnostics.seas-res-f", "diagnostics.seas-res-qs",
                            "diagnostics.seas-res-kw", "diagnostics.seas-res-friedman", "diagnostics.seas-res-periodogram",
                            "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-i-f",
                            "diagnostics.seas-i-qs", "diagnostics.seas-i-kw", "diagnostics.seas-i-periodogram",
                            "diagnostics.seas-i-friedman", "diagnostics.seas-i-spectralpeaks",
                            "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw",
                            "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram",
                            "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-lin-f",
                            "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw", "diagnostics.seas-lin-friedman",
                            "diagnostics.seas-lin-periodogram", "diagnostics.seas-lin-spectralpeaks",
                            "diagnostics.seas-sa-ac1", "diagnostics.td-sa-all", "diagnostics.td-sa-last",
                            "diagnostics.td-i-all", "diagnostics.td-i-last", "diagnostics.td-res-all",
                            "diagnostics.td-res-last", "diagnostics.seas-lin-combined", "diagnostics.seas-lin-evolutive",
                            "diagnostics.seas-lin-stable", "diagnostics.seas-si-combined",
                            "diagnostics.seas-si-combined3", "diagnostics.seas-si-evolutive",
                            "diagnostics.seas-si-stable", "diagnostics.seas-res-combined",
                            "diagnostics.seas-res-combined3", "diagnostics.seas-res-evolutive",
                            "diagnostics.seas-res-stable", "diagnostics.seas-i-combined",
                            "diagnostics.seas-i-combined3", "diagnostics.seas-i-evolutive",
                            "diagnostics.seas-i-stable", "diagnostics.seas-sa-combined",
                            "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive",
                            "diagnostics.seas-sa-stable", "diagnostics.fcast-insample-mean",
                            "diagnostics.fcast-outsample-mean", "diagnostics.fcast-outsample-variance",
                            "variancedecomposition.cycle", "variancedecomposition.seasonality",
                            "variancedecomposition.irregular", "variancedecomposition.tdh",
                            "variancedecomposition.others", "variancedecomposition.total",
                            "seats.parameters_cutoff", "seats.model_changed", "seats.seasonality",
                            "seats.ar_root(*)", "seats.ma_root(*)", "seats.tvar-estimator",
                            "seats.tvar-estimate", "seats.tvar-pvalue", "seats.savar-estimator",
                            "seats.savar-estimate", "seats.savar-pvalue", "seats.svar-estimator",
                            "seats.svar-estimate", "seats.svar-pvalue", "seats.ivar-estimator",
                            "seats.ivar-estimate", "seats.ivar-pvalue", "seats.tscorr-estimator",
                            "seats.tscorr-estimate", "seats.tscorr-pvalue", "seats.ticorr-estimator",
                            "seats.ticorr-estimate", "seats.ticorr-pvalue", "seats.sicorr-estimator",
                            "seats.sicorr-estimate", "seats.sicorr-pvalue"), dim = 327L)

# jts<-rjd3toolkit::ts_r2jd(rjd3toolkit::ABS$X0.2.09.10.M)
# jrslt<- rJava::.jcall("demetra/tramoseats/r/Tramo", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, "trfull")
# rjd3toolkit::dictionary(rjd3toolkit::jd3Object(jrslt, result = TRUE)) |>
#   dput()

sa_tramo = structure(c("arima.phi(*)", "arima.bphi(*)", "arima.theta(*)",
                       "arima.btheta(*)", "y_f(?)", "y_b(?)", "y_ef(?)", "y_eb(?)",
                       "yc", "l", "ycal", "det", "det_f(?)", "det_b(?)", "cal", "cal_f(?)",
                       "cal_b(?)", "tde", "tde_f(?)", "tde_b(?)", "mhe", "mhe_f(?)",
                       "mhe_b(?)", "ee", "ee_f(?)", "ee_b(?)", "omhe", "omhe_f(?)",
                       "omhe_b(?)", "out", "out_f(?)", "out_b(?)", "regression.mu",
                       "regression.lp", "regression.easter", "regression.outlier(*)",
                       "regression.td(*)", "regression.user(*)", "regression.out(*)",
                       "regression.missing(*)", "regression.ml.pcorr", "residuals.ser",
                       "residuals.res", "residuals.tsres", "arima.p", "arima.d", "arima.q",
                       "arima.bp", "arima.bd", "arima.bq", "y", "period", "span.start",
                       "span.end", "span.n", "span.missing", "log", "adjust", "regression.espan.start",
                       "regression.espan.end", "regression.espan.n", "regression.espan.missing",
                       "regression.mean", "regression.nlp", "regression.ntd", "regression.nmh",
                       "regression.nout", "regression.nao", "regression.nls", "regression.ntc",
                       "regression.nso", "regression.nusers", "regression.leaster",
                       "regression.description", "regression.type", "regression.details.coefficients",
                       "regression.details.covar", "regression.details.covar-ml", "regression.ml.parameters",
                       "regression.ml.pcovar", "regression.ml.pcovar-ml", "regression.ml.pscore",
                       "likelihood.ll", "likelihood.adjustedll", "likelihood.ssqerr",
                       "likelihood.nparams", "likelihood.nobs", "likelihood.neffectiveobs",
                       "likelihood.df", "likelihood.aic", "likelihood.aicc", "likelihood.bic",
                       "likelihood.bicc", "likelihood.bic2", "likelihood.hannanquinn",
                       "residuals.type", "residuals.mean", "residuals.doornikhansen",
                       "residuals.skewness", "residuals.kurtosis", "residuals.lb", "residuals.bp",
                       "residuals.seaslb", "residuals.seasbp", "residuals.lb2", "residuals.bp2",
                       "residuals.nruns", "residuals.lruns", "residuals.nudruns", "residuals.ludruns",
                       "out_i", "out_i_f(?)", "out_i_b(?)", "out_t", "out_t_f(?)", "out_t_b(?)",
                       "out_s", "out_s_f(?)", "out_s_b(?)", "reg_i", "reg_i_f(?)", "reg_i_b(?)",
                       "reg_t", "reg_t_f(?)", "reg_t_b(?)", "reg_s", "reg_s_f(?)", "reg_s_b(?)",
                       "reg_sa", "reg_sa_f(?)", "reg_sa_b(?)", "reg_y", "reg_y_f(?)",
                       "reg_y_b(?)", "reg_u", "reg_u_f(?)", "reg_u_b(?)", "det_i", "det_i_f(?)",
                       "det_i_b(?)", "det_t", "det_t_f(?)", "det_t_b(?)", "det_s", "det_s_f(?)",
                       "det_s_b(?)"), dim = 146L)
switch (x,
        tramoseats = sa_tramoseats,
        tramo = sa_tramo
)
}
