#'@importFrom stats printCoefmat end time
#'@importFrom utils capture.output
print_diagnostics <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...){
  diagnostics = rjd3toolkit::diagnostics(x)
  variance_decomposition = diagnostics$variance_decomposition
  residuals_test = diagnostics$residuals_test

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
               printCoefmat(residuals_test[,"P.value", drop = FALSE], digits = digits,
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
print.JD3_TRAMOSEATS_OUTPUT<- function(x, digits = max(3L, getOption("digits") - 3L),
                                ...){
  print(x$result, digits = digits, ...)
}

#' @export
plot.JD3_TRAMOSEATS_RSLTS <- function(x, first_date = NULL, last_date = NULL,
                               type_chart = c("sa-trend", "seas-irr"),
                               caption = c("sa-trend" = "Y, Sa, trend",
                                           "seas-irr" = "Sea., irr.")[type_chart],
                               colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                          s = "#1E6C0B", i = "#155692"),
                               ...){
  plot(rjd3sa::sa.decomposition(x),
       first_date = first_date, last_date = last_date,
       type_chart = type_chart,
       caption = caption,
       colors = colors,
       ...)
}
#' @export
plot.JD3_TRAMOSEATS_OUTPUT <- function(x, first_date = NULL, last_date = NULL,
                                type_chart = c("sa-trend", "seas-irr"),
                                caption = c("sa-trend" = "Y, Sa, trend",
                                            "seas-irr" = "Sea., irr.")[type_chart],
                                colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                           s = "#1E6C0B", i = "#155692"),
                                ...){
  plot(x$result,
       first_date = first_date, last_date = last_date,
       type_chart = type_chart,
       caption = caption,
       colors = colors,
       ...)
}

#' @importFrom rjd3toolkit diagnostics
#' @export
diagnostics.JD3_TRAMOSEATS_RSLTS<-function(x, ...){
  if (is.null(x)) return (NULL)
  variance_decomposition = x$diagnostics$vardecomposition
  variance_decomposition = matrix(unlist(variance_decomposition),
                                  ncol = 1,
                                  dimnames = list(names(variance_decomposition), "Component"))
  residuals_test = x$diagnostics[grep("test", names(x$diagnostics))]
  residuals_test = data.frame(Statistic = sapply(residuals_test, function(test) test[["value"]]),
                              P.value = sapply(residuals_test, function(test) test[["pvalue"]]),
                              Description = sapply(residuals_test, function(test) test[["description"]]))
  list(variance_decomposition = variance_decomposition,
       residuals_test = residuals_test)
}

#' @export
diagnostics.JD3_TRAMOSEATS_OUTPUT<-function(x, ...){
  return (rjd3toolkit::diagnostics(x$result, ...))
}

