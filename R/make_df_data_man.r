#' @title \code{make_df} method for \code{data_man_container} object
#' @description ...
#' @param object a \code{data_man_container} object
#' @param env useless here (included for S3 method consistency.)
#' @param ... useless here (included for S3 method consistency.)
#' @param where if \code{TRUE} then 'where' info is included. If \code{NULL}
#'   (default) then the function will guess that you want it if there is such
#'   information.
#' @param comment if \code{TRUE}  then 'comment' info is included. If
#'   \code{NULL} (default) then the function will guess that you want.
#' @param rgroup if \code{TRUE}  then a \code{cr_group} object is returned with
#'   attribute \code{rgroup} from 'rgroup' info. If \code{NULL} (default) then
#'   the function will guess that you want
#' @param null.rgroup the \code{rgroup} attribute for \code{is.na(rgroup)}.
#' @export

make_df.data_man <- function(object, env, ..., where = NULL, comment = NULL, rgroup = NULL, null.rgroup = "Other"){
    if(is.null(rgroup)) {
        rgroup <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$rg))))) TRUE else FALSE
    }
    if(is.null(comment)) {
        comment <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$comment))))) TRUE else FALSE
    }
    if(is.null(where)) {
        where <- if(any(unlist(lapply(object, FUN = function(x) !is.null(x$where))))) TRUE else FALSE
    }
    if(!missing(env)) {
        warning("[make_df.data_man] argument 'env' does not do anything...\n (Included for S3 method consistency.)")
    }
    n_col = 2 + where + comment
    M <- matrix(NA_character_, nrow=length(object), ncol=n_col)
    colnames(M) <- c(
        "variable",
        "database entry",
        if(where) "from" else NULL,
        if(comment) "comment" else NULL
    )
    rgr <- NULL
    for(k in seq_along(object)){ # k  = 1
        M[k, 1] <- object[[k]]$name
        M[k, 2] <- if(is.na(object[[k]]$var)) "?" else object[[k]]$var
        if(where) M[k, 3] <- if(is.na(object[[k]]$where)) "?" else object[[k]]$where
        if(comment) {
            the_comment <- object[[k]]$comment
            M[k, 3 + where] <- if(is.null(the_comment)) "" else the_comment
        }
        if(rgroup){
            the_rg <-  object[[k]]$rg
            rgr <- c(rgr, if(is.null(the_rg)) null.rgroup else the_rg)
        }
    }
    M <- as.data.frame(M)
    if(rgroup) cr_group(M, rgroup = rgr) else M
}
