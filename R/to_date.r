#' @title Transform to date
#' @description wrapper for \code{as.Date} that replaces ""-strings with
#' \code{NA} before conversion
#' @param x character or factor
#' @param not_dates the things that should be NA
#' @param ... arguments passed to \code{as.Date}
#' @export
to_date <- function(x, not_dates = c("", " "), ...){
   if(is.factor(x)) x <- as.character(x)
   x[x %in% not_dates] <- NA
   as.Date(x, ...)
}
