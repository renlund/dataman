##' elements of uniformal length
##'
##' make nchar have the same value for all elements of x
##' @title fixed length
##' @param x vector to fix
##' @param n uniformal nchar length for elements of x
##' @param fill character to fill the void
##' @param right fill to the right?
##' @return character string
##' @author Henrik Renlund
##' @export
##' @examples
##' fixed_length(x = c("A", "ABC"))
##' fixed_length(x = c("A", "ABC"), 2, fill = "_")
##' fixed_length(x = c("A", "ABC"), fill = ".", right = FALSE)
##' fixed_length(8:12, fill = "0")

fixed_length <- function(x, n = NULL, fill = " ", right = TRUE){
    if(nchar(fill)>1) fill <- substr(fill, 1, 1)
    y <- as.character(x)
    if(is.null(n)) n <- max(nchar(y))
    z <- y
    for(k in seq_along(y)){
        m <- nchar(y[k])
        if(m<n){
            z[k] <- if(right){
                paste0(paste(rep(fill, n-m), collapse = ""), y[k])
            } else {
                paste0(y[k], paste(rep(fill, n-m), collapse = ""))
            }
        } else {
            z[k] <- substr(y[k], 1, n)
        }
    }
    z
}


