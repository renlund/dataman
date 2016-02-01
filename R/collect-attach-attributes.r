#' @title Collect attributes
#' @description Since attributes sometimes gets lost in R functions, these are
#'   some convience functions to collect and attach attributes from object.
#' @param df data frame or similar object
#' @param attr attribute
#' @author Henrik Renlund
#' @examples
#' df1 <- data.frame(id = 1:2, x = runif(2))
#' attr(df1$x, "label") <- "X value"
#' df2 <- data.frame(id = 2:1, y = runif(2))
#' attr(df2$id, "label") <- "Identifier"
#' attr(df2$y,  "label") <- "Y value"
#' df <- merge(df1, df2, by = "id")
#' str(df) ## no attributes on variables
#' c1 <- collect_attr(df1)
#' c2 <- collect_attr(df2)
#' df <- attach_attr(df, c(c1,c2))
#' str(df) ## has attributes on variables
#' rm(df1, df2, c1, c2, df)
#' @export

collect_attr <- function(df, attr = "label"){
    fnc <- function(x) attr(x = x, which = attr)
    unlist(lapply(df, fnc))
}

#' @title Attach attributes
#' @param df data frame or similar object
#' @param catcher named vector of attributes
#' @param attr attribute
#' @author Henrik Renlund
#' @seealso collect_attr
#' @export

attach_attr <- function(df, catcher, attr = "label"){
    for(K in names(catcher)){
        if(!K %in% names(df)) next
        attr(x = df[[K]], which = attr) <- as.character(catcher[K])
    }
    df
}
