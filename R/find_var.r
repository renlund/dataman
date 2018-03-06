#' @title Find variable
#'
#' @description Look for a pattern in the \code{names} of R objects (typically data frames) and
#' return the hits
#'
#' @details This function applies \code{\link{grepr}} to \code{\link{names}} of a set
#' of R objects and returns the results as a list
#'
#' @author Henrik Renlund
#' @param pattern pattern to look for
#' @param dfs logical or character string. If a character vector of the names of
#' R objects is given, the \code{names} of these will be examined with \code{grepr}
#' ; if \code{TRUE} all data frames in the global environment will be examined and if
#' \code{FALSE} all objects with a 'names' attribute will be examined.
#' @param ignore.case logical; should upper/lower case distinction be ignored? (default \code{TRUE})
#' @param ... arguments to be passed to \code{\link{grep}}
#' @examples
#' require(datasets)
#' find_var(pattern="a", dfs=c("mtcars", "esoph"), index=TRUE)
#' @seealso \code{\link{grepr}}, \code{\link{grep}}
#' @return a list
#' @export

find_var <- function(pattern, dfs=TRUE, ignore.case=TRUE, ...) {
   if(is.logical(dfs)){
      tmp <- c()
      if(dfs) {
         for(K in ls(.GlobalEnv))
            if( "data.frame" %in% class(get(K, envir = .GlobalEnv))) tmp <- c(K, tmp)
      } else {
         for(K in ls(.GlobalEnv))
            if(!is.null(dummy <- attr(get(K, envir = .GlobalEnv), "names"))) tmp <- c(K, tmp)
      }
      dfs <- tmp
   }
   L <- NULL
   class(L) <- "list"
   for(df in dfs){
      if(!is.null(var.names <- names(get(df, envir = .GlobalEnv)))){
         L[[df]] <- grepr(pattern, var.names, ignore.case = ignore.case, ...)
      } else {
         warning(paste0("[find_var] Object ", df, " does not have any names."))
      }
   }
   L
}

##' get variables and labels
##'
##' get a data frame containing the variable and associated labels that are
##'     associdated with some data set
##' @param df a data frame (or similar object)
##' @param name optionally, the name of the data frame for output
##' @return a data frame
##' @export
var_lab <- function(df, name = as.character(substitute(df))){
    foo <- function(x){
        if(is.null(r <- attr(x, "label"))) "" else r
    }
    data.frame(
        dataframe = name,
        variable = names(df),
        label = unlist(lapply(df, foo)),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

##' @describeIn find_var \code{find_info} only works on data frames (and similar
##'     objects) and looks at variable names as well as possible labels
##' @param verbose logical, show messages?
##' @export
find_info <- function(pattern, ignore.case = TRUE, ..., verbose = TRUE){
    R <- NULL
    for(K in ls(.GlobalEnv)){
        if( "data.frame" %in% class(get(K, envir = .GlobalEnv))){
            tmp <- subset(var_lab(get(K, envir = .GlobalEnv), name = K),
                          subset = grepl(pattern = pattern, x = variable,
                                         ignore.case = ignore.case, ... ) |
                              grepl(pattern = pattern, x = label,
                                    ignore.case = ignore.case, ... ))
            R <- if(is.null(R)) tmp else rbind(R, tmp)
        } else next
    }
    if(nrow(R) == 0){
        if(verbose) message("find_info found nothing")
        invisible(as.data.frame(NULL))
    } else {
        if(verbose) message("find_info found:")
        R
    }
}

if(FALSE){

    AFG <- data.frame(
        arg = 1,
        boo = "a"
    )
    attr(AFG$arg, "label") <- "a label of sorts"
    BRT <- data.frame(
        farg = 2,
        bobb = "b"
    )
    attr(BRT$bobb, "label") <- "another label of sorts"
    attr(BRT$farg, "label") <- "lots if info"

    var_lab(AFG)
    var_lab(BRT)
    find_info("or")
    find_info("^foo")
    find_var("o")

    rm(AFG, BRT)

}
