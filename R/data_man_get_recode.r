#' @title Show \code{data_man} recodings
#' @description ...
#' @param container the 'data_man' container
#' @param file 'file' argument for \code{Hmisc::latex} (default = "")
#' @param lab.prefix prefix for labels for LaTeX tables (default = "tab:recode_") which concatenates with the variable name
#' @param ... arguments passed to \code{Hmisc::latex}.
#' @author Henrik Renlund
#' @export

data_man_get_recode <- function(container, file = "", lab.prefix = "tab:recode_", ...){
   if(missing(container)){
      tryCatch(expr = container <- get("data_man_container", envir = .GlobalEnv),
               error = function(e) stop("[data_man_get_recode] no default container available"))
   }
   ttify <- function(s) if(length(s)) gsub("_", "\\_", paste0("\\texttt{", s, "}"), fixed=TRUE ) else ""
   for(k in seq_along(container)){
      X <- container[[k]]

      if(is.null(recode <- X$recode)){
         next
      } else {
         if(is.null(L <- X$recode_table)){
            tmp_var <- get(X$where)[[X$var]]
            L <- table(
               tmp_var,
               refactor(x = tmp_var, L = recode),
               dnn = c(X$var, X$name),
               useNA="ifany"
            )
         }
      }
      var <- ttify(X$where)
      Hmisc::latex(object = L,
                   file = file,
                   append = TRUE,
                   caption = paste0("Recoding of data base ",ttify(X$where), " entry ",ttify(X$var)," into ",ttify(X$name),"."),
                   label = paste0(lab.prefix, X$name),
                   ...)
   }
   invisible(NULL)
}
