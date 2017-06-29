##' table wrapper
##'
##' table for variables in a data frame
##' @param data a data frame (or similar)
##' @param ... variables (unquoted)
##' @return a table of the variables
##' @export
dftable <- function(data, ...){
    L <- as.character(eval(substitute(alist(...))))
    M <- paste(L, collapse = ", ")
    code <- paste0("with(data, table(", M, ", useNA = 'always'))")
    eval(parse(text = code))
}

if(FALSE){
    n <- 100
    X <- data.frame(
        x = sample(LETTERS[1:4], size = n, replace = TRUE),
        y = sample(letters[5:8], size = n, replace = TRUE),
        z = sample(0:1, size = n, replace = TRUE)
    )
    dftable(data = X, x)
    dftable(X, x, y)
    dftable(X, x, y, z)
}
