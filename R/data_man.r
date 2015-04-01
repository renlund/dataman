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
                     var = NA,
                     where = NA,
                     recode = NULL,
                     date = NULL,
                     rg = NULL,
                     comment = NULL,
                     container = "data_man_container",
                     label = FALSE,
                     check = TRUE
){
    if(is.na(where)){
        if(check) message("[data_man] cannot 'check' if arg 'where' is missing")
        check <- FALSE
    }
    if(check){
        if(!exists(var, get(where))) warning("[data_man] data base entry appears to be non-existing")
        tmp_var <- get(where)[[var]]
        class_var <- class(tmp_var)
        cat(paste(rep("-", options("width")[[1]]-3), collapse=""), "\n")
        cat("Adding data base '", where,"' entry '", var, "' as variable '", name,"'\n", sep = "")
        cat("A variable of class: ", class_var, "\n")
        #if(!any(class_var %in% c("Date", "POSIXct", "POSIXt"))){
        if(class_var %in% c("numeric", "integer")){
            cat("\nSummary of numeric variable:")
            cat("\n    min:", min(tmp_var, na.rm=T), "\n    max:", max(tmp_var, na.rm=TRUE), "\n    mean:",mean(tmp_var, na.rm=TRUE), "\n")
        }
        if(class_var %in% c("integer", "numeric", "factor", "character")){
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
                if(length(x) & !class_var %in% c("numeric", "integer") ){
                    cat("\nThe first (at most 20) non-NA or non-empty values are:\n   ", paste0(x, collapse =", "), "\n")
                } else if(!length(x)){
                    cat("There are only NA or empty values!\n")
                }
            }
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
        label = if(is.character(label)) label else if(label) var else NULL,
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
