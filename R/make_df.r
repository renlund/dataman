#' @title Turn objects into data frames
#' @description Some R functionality (I'm thinking mainly of ggplot2) requires
#' the data you are interested in to be packaged in data frames, hence this
#' function.
#' @param object an object
#' @param env an environment
#' @param ... passed arguments
#' @author Henrik Renlund
#' @importFrom survival Surv survfit
#' @examples
#' # combine variables into a data frame
#' x <- 1:2
#' y <- letters[1:2]
#' make_df(c("x","y"))
#' # 'env' is R_GlobalEnv by default but can be any environment, data frame or list
#' make_df(c("mpg", "disp"), env=datasets::mtcars)
#' # methods include 'list' ...
#' make_df(list(x=1:2, y=letters[1:2]))
#' # and 'survfit'
#' library(survival)
#' n <- 50
#' s <- Surv(rexp(n), rbinom(n,1,0.2))
#' f <- factor(rep(letters[1:2], length.out=n))
#' sf <- survfit(s~f)
#' make_df(sf)
#' @export

make_df <- function(object, env, ...) UseMethod("make_df")

# - # @title Turn objects into data frames
# - # @param object an object
# - # @param env an environment
# - # @param ... passed arguments
# - # @author Henrik Renlund
#' @export

make_df.default <- function(object, env, ...){
  message("The default method of make_df doesn't do much")
  as.data.frame(NULL)
}

# - # @title Turn objects into data frames
# - # @param object an object
# - # @param env an environment
# - # @param ... passed arguments
# - # @author Henrik Renlund
#' @export
make_df.data.frame <- function(object, env, ...){
  object
}

# - # @title Turn variables into a data frame
# - # @param object a character vector of variables contained in \code{env}
# - # @param env the environment, data frame or list that contains the variables
# - # @param ... for future needs?
# - # @author Henrik Renlund
#' @export

make_df.character <- function(object, env=.GlobalEnv, ...){
#    if( class(env) %in% c("data.frame", "list")){
#       env <- get(as.character(substitute(env)), inherits=TRUE)
#    }
  if(any(indx<-table(object)>1)) warning(paste0("[make_df.character] the object contains multiplicities (check entries: ",paste0(names(table(object)[indx]), collapse=", "),")"))
   for(K in object){
      if(!exists(K, env)){
         stop(paste0("'",K,"' does not exist in the specified place (environment, data frame, or list)."))
      } else {
         klass <- class(get(K, env))
         if(!exists("klasser", inherits=FALSE)) klasser <- klass
         klasser <- intersect(klass, klasser)
      }
   }
   curr_extensions <- setdiff(gsub("make_df.", "", methods("make_df")), c("default", "character"))
   if(length(klasser)>0){
      if(any(klasser %in% curr_extensions)){
         # do we need to force all items in object to have the same class?
         for(k in seq_along(object)){
            X <- make_df(get(object[k],env))
            if(is.null(d <- nrow(X))) d <- 1
            if(length(object)>1) X <- cbind(X, "object" =rep(k, d))
            rX <- if(!exists("rX",inherits=FALSE)){
               X
            } else {
              tryCatch(expr = rbind(rX, X),
                       error = function(e) stop(paste0("[make_df.character] could not rbind the make_df:ified '", object[k],"' (from the specified enviroment) to the rbind-accumulation of: ", paste(object[1:(k-1)], collapse=", "), ".")))
            }
         }
         return(rX)
      }
   }
   code <- paste0("data.frame(", paste0("'", object, "' = get('",object,"', env)", collapse=", "), ")")
   eval(parse(text=code))
}

# make_df(1)
# x <- 1:5; y <- LETTERS[1:5]; z <- factor(rep(letters[2:1], len=5))
# (df <- make_df(object=c("x", "y", "z")))
# make_df(c("x", "z"), df)

# - # @title Turn survfit object into a data frame
# - # @param object a survfit object
# - # @param env environment must currently be set to \code{.GlobalEnv}
# - # @param ... for future needs?
# - # @author Henrik Renlund
#' @export

make_df.survfit <- function(object, env=.GlobalEnv, ...){
   if(environmentName(env)!="R_GlobalEnv"){
      stop("[make_df.survfit] environments (other than the global) are currently unsupported")
   }
   use <- c("time", "n.risk", "surv", "upper", "lower")
   if(!is.null(strata <- object[['strata']])){
      namn <- names(strata)
      object[['strata']] <- factor(rep(namn, strata), levels=namn)
      use <- c(use, "strata")
   }
   make_df(object=use, env=object)
}

# library(survival)
# S <- Surv(time=rexp(50,1), event=rbinom(50,1,0.2))
# sf <- survfit(S~1)
# sf2 <- sf
#
# make_df(object=  sf)
# make_df(object = "sf")
# make_df(object = c("sf", "sf2"))

# - # @title Turn elements of a list into a data frame
# - # @param object a list of objects for which a make_df method exist
# - # @param env environment must currently be set to \code{.GlobalEnv}
# - # @param ... for future needs?
# - # @author Henrik Renlund
#' @export

make_df.list <- function(object, env=.GlobalEnv, ...){
   if(environmentName(env)!="R_GlobalEnv"){
      stop("[make_df.list] environments (other than the global) are currently unsupported")
   }
   use <- names(object)
   make_df(object=use, env=object)
}
