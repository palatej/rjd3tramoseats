#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @import RProtoBuf
NULL

#' Java Utility Functions
#'
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#'
#' @param jtest,s,period,startYear,startPeriod,length,ldt,dt,jparams,start,end,y,m,d,year,month,day,type,p,code,prefix,r,span,rspan,rslt,name,jobj,jrslt parameters.
#'
#' @name jd3_utilities
NULL
#> NULL

identical_na <- function(x){
  identical(x, NA) ||
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}
