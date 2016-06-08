#' @title Get data from \code{data_man_container} info
#' @description ...
#' @param id vector of id's
#' @param container name of \code{data_man_container} object
#' @param id.name identifier across data bases or a list of such
#' @param na.where if entry \code{where} is \code{NA} in
#' @param dplyr use dplyr for merging operations? (Default: \code{TRUE}.)
#' \code{data_man_container}, this object is used in its place.
#' @export

data_man_create <- function(id,
                            container,
                            id.name,
                            na.where = NULL,
                            dplyr = TRUE
){
    # if(missing(id.name)) id.name <- "id"
    if(!requireNamespace("dplyr")){
        warning("[data_man::data_man_create] package dplyr not available")
        dplyr <- FALSE
    }
    if(missing(container)){
        tryCatch(expr = container <- get("data_man_container", envir = .GlobalEnv),
                 error = function(e) stop("[data_man_create] no default container available"))
    }
    identicalid <- length(id.name) == 1
    the_id <- id.name[[1]]
    if(!is.null(na.where)){
        tryCatch(expr = NAW <- get(na.where, envir = .GlobalEnv),
                 error = function(e) stop("[data_man_create] can't find 'na.where'"))
        if(!identicalid) id.name[["NAW"]] <- id.name[[na.where]]
    }
    DF <- if(dplyr) dplyr::data_frame(id) else data.frame(id)
    names(DF) <- the_id

    for(indx in seq_along(container)){ # indx <- 17
        cat(paste0("Fixing variable no.", indx, ": ", names(container)[indx], "\n"))
        X <- container[[indx]]
        var <- X$var
        name <- X$name
        if( is.na(X$where) & is.null(na.where)){
            next
        } else {
            df <- if(!is.na(X$where)) X$where else "NAW"
            tmp <- get(df)[[var]]
        }
        if(!is.null(f <- X$date)){
            tmp <- get(f)(tmp)
        }
        if(!is.null(recode <- X$recode)){
            tryCatch(expr = {
                tmp <- refactor(x=tmp, L=recode)
            },
            warning=function(w) {
                print(var)
                print(w)
                # break
            }
            )
        }
        if(dplyr){
            loc.df <- dplyr::data_frame(
                tmp,
                get(df)[[if(identicalid) the_id else id.name[df][[1]]]]
            )
            names(loc.df) <- c(name, the_id)
            ## DF <- dplyr::inner_join(DF, loc.df, by = the_id)
            DF <- dplyr::full_join(DF, loc.df, by = the_id)
            if(!is.null(X$label)) attr(DF[[name]], "label") <- X$label
        } else{
            loc.df <- data.frame(
                tmp,
                get(df)[[if(identicalid) the_id else id.name[df][[1]]]]
            )
            names(loc.df) <- c(name, the_id)
            ## DF <- merge(DF, loc.df, by.x = the_id)
            DF <- merge(DF, loc.df, by.x = the_id, all = TRUE)
            if(!is.null(X$label)) attr(DF[[name]], "label") <- X$label
        }
    }
    DF
}
