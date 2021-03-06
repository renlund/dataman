#' @title Data Management Tool (interactive-ish)
#' @description This creates a list of data management information (defaults to
#' a 'data_man' object called 'data_man_container' in the global workspace).
#' This information can then be used to extract and recode (factors and dates)
#' for (or towards) an analytical data base.
#' @param name (new) name of variable
#' @param var database entry
#' @param where database (or data frame)
#' @param recode refactor L argument for recoding
#' @param date function for transformation into date format
#' @param rg rgroup label
#' @param comment variable comment
#' @param container name of container of information in \code{.GlobalEnv}.
#'   Defaults to "data_man_container"
#' @param label if TRUE then the data base entry will be stored as a label for
#'   the new variable. Another label can be specified with a character string.
#' @param check if TRUE this checks the variable
#' @author Henrik Renlund
#' @export

data_man <- function(name,
                     var = name,
                     where = NA,
                     recode = NULL,
                     date = NULL,
                     rg = NULL,
                     comment = NULL,
                     container = "data_man_container",
                     label = NULL,
                     check = TRUE
){
    if(is.na(where)){
        if(check) message("[data_man] cannot 'check' if arg 'where' is missing")
        check <- FALSE
    }
    the_label <- label
    if(check){
        if(!exists(var, get(where))) warning("[data_man] data base entry appears to be non-existing")
        tmp_var <- get(where)[[var]]
        ## if(is.null(tmp_var)){
        ##     warning(paste0("no variable '", var, "' in database '",
        ##                    where, "'."))
        ##     return(invisible(NULL))
        ## }
        class_var <- class(tmp_var)
        the_label <- if(is.null(label)) {
                         if(!is.null(var_lab <- attr(tmp_var, "label"))){
                             var_lab
                         } else {
                             var
                         }
                     } else label
        cat(paste(rep("-", options("width")[[1]]-3), collapse=""), "\n")
        cat("Adding data base '", where,"' entry '", var, "' as variable '", name,"'\n", sep = "")
        cat("A variable of class: ", paste(class_var, collapse = ";"), "\n")
        #if(!any(class_var %in% c("Date", "POSIXct", "POSIXt"))){
        n_miss <- sum(is.na(tmp_var))
        perc_miss <- signif(100 * n_miss / length(tmp_var), 2)
        cat(paste0("There are ", n_miss, " (",perc_miss,"%) missing.\n"))
        if(any(class_var %in% c("numeric", "integer"))){
            cat("\nSummary of numeric variable:")
            cat("\n    min:", min(tmp_var, na.rm=T), "\n    max:", max(tmp_var, na.rm=TRUE), "\n    mean:",mean(tmp_var, na.rm=TRUE), "\n")
        }
        if(any(class_var %in% c("integer", "numeric", "factor", "character"))){
            n_unique <- length(unique(tmp_var))
            if(n_unique<=10){
                cat("\nThere are only", n_unique,"unique values. \nTabulated: \n\n")
                print(table(tmp_var, useNA="always", dnn = NULL))
            } else {
                x <- tmp_var[!is.na(tmp_var)]
                if(!is.numeric(tmp_var)){
                    x <- x[!grepl("^ *$", x)]
                }
                x <- x[1:min(length(x), 20)]
                if(length(x) & !any(class_var %in% c("numeric", "integer")) ){
                    cat("\nThe first (at most 20) non-NA or non-empty values are:\n   ", paste0(x, collapse =", "), "\n")
                } else if(!length(x)){
                    cat("There are only NA or empty values!\n")
                }
            }
        }
        if(any(class_var %in% c("Date", "POSIXct"))){
            cat("\nDates span from min =", as.character(min(tmp_var, na.rm = TRUE)),
                "to max = ", as.character(max(tmp_var, na.rm = TRUE)), "\n")
        }
        if(!is.null(recode)){
            cat("\nCross-tabulating the recoding: \n\n")
            print(
                recode_table <- table(
                    tmp_var,
                    refactor(x = tmp_var, L = recode),
                    dnn = c(var, name),
                    useNA="ifany"
                )
            )
        }
    }
    if(!is.character(container)) stop("[data_man] wrong 'container' argument")
    if(
        length(container)>1 |
            nchar(container)==0 |
            grepl(" ", container) |
            grepl("^[0-9]", container)
    ) stop("[data_man] wrong 'container' argument")
    L <- list(
        name = name,
        var = var,
        where = where,
        recode = recode,
        date = date,
        rg = rg,
        comment = comment,
        label = the_label,
        recode_table = if(exists("recode_table", envir = environment(), inherits = FALSE)) recode_table else NULL
    )
    if(!exists(container, envir=.GlobalEnv)) {
        foo <- as.list(NULL)
        class(foo) <- "data_man"
    } else {
        foo <- get(container, envir = .GlobalEnv)
        if(
            !is.list(foo) | !"data_man" %in% class(foo) # |
            # length(names(foo)) != length(foo) |
            # any(unlist(lapply(foo, function(x) names(x)[1] != "name")))
        ){
            cat(paste0("A variable '", container, "' already exists and does not appear to be correct. Would you like to overwrite it?\n"))
            if(readline("Press 'x' to abort, anything else to proceed\n  ") == "x"){
                return(invisible(NULL))
            }
            foo <- as.list(NULL)
            class(foo) <- "data_man"
        }
    }
    foo[[name]] <- L
    assign(container, foo, envir = .GlobalEnv)
}
