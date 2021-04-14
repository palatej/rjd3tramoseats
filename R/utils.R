identical_na <- function(x){
  identical(x, NA) ||
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}

ymd<-function(y, m, d=1){
  return (as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}

yearOf<-function(s){
  return ( as.integer(substr(s, 1, 4)))
}

monthOf<-function(s){
  return ( as.integer(substr(s, 6, 7)))
}

dayOf<-function(s){
  return ( as.integer(substr(s, 9, 10)))
}

fixedParameters<-function(coef){
  if (length(coef) == 0) return (NULL)
  return (lapply(coef, function(z){list(value=z, type="FIXED")}))
}

