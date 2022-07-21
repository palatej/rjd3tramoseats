#'@importFrom stats printCoefmat
#'@importFrom utils capture.output
print_diagnostics <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...){
  variance_decomposition = x$diagnostics$vardecomposition
  variance_decomposition = matrix(unlist(variance_decomposition),
                                  ncol = 1,
                                  dimnames = list(names(variance_decomposition), "Component"))
  residuals_test = x$diagnostics[grep("test", names(x$diagnostics))]
  residuals_test = lapply(residuals_test,function(test) test[["pvalue"]])
  residuals_test = matrix(unlist(residuals_test),
                          ncol = 1,
                          dimnames = list(names(residuals_test), "P.value"))

  cat("Relative contribution of the components to the stationary",
      "portion of the variance in the original series,",
      "after the removal of the long term trend (in %)",
      sep = "\n"
  )
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(variance_decomposition*100, digits = digits, ...)
             )),
      sep ="\n")
  cat("\n")

  cat("Residual seasonality tests")
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(residuals_test, digits = digits,
                            na.print = "NA", ...)
             )
  ),
  sep ="\n")
  cat("\n")

  invisible(x)
}
print_final <- function(x, ...){
  print(rjd3sa::sa.decomposition(x), ...)
  invisible(x)
}

#' @export
print.JD3_TRAMOSEATS_RSLTS <- function(x, digits = max(3L, getOption("digits") - 3L),
                                ...){

  cat("TRAMO","\n",sep="")
  print(x$preprocessing, digits = digits, ...)
  cat("\n", "Decomposition","\n",sep="")
  print(x$decomposition$canonicaldecomposition, ...)
  cat("\n", "Diagnostics","\n",sep="")
  print_diagnostics(x, digits = digits, ...)
  cat("\n", "Final","\n",sep="")
  print_final(x, digits = digits, ...)
}
#' @export
print.JD3TRAMOSEATS_OUTPUT<- function(x, digits = max(3L, getOption("digits") - 3L),
                                ...){
  print(x$result, digits = digits, ...)
}

