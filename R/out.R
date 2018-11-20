
#' Main Generic function to output variable
#' Render an object to the ouput
#' @export
out <- function(x, title=deparse(substitute(x)), ...) {
  if( is_debug() ) {
    cat("[out] ",paste(class(x), collapse=' '),"\n")
  }
  apply_handlers(x, title=title, ...)
  UseMethod("out")
}

#' Default render fo an object
#' @method out default
#' @param x data
#' @param name export name, calls .output.file
#' @param title title of the output
#' @param main for compat used instead of title
#' @param graph uses .output.graph handler
#' @export
#'
out.default <- function(x, name=NULL, title="", main="", ... ) {
  title = ifelse(!is.null(main) && main != "", main, ifelse(is.null(title),"",title))
  if( is_debug() ) {
    cat("[out.default] ", paste(class(x), collapse=' '),"\n")
    cat("title=", title, "main=",main, "\n")
  }
  xprint(x, title=title, ...)
  invisible()
}

#' out for factors
#' @method out factor
#' @export
out.factor <- function(x, title="", ...) {
  if( is_debug() ) {
    cat("[out.factor] ",paste(class(x),collapse=' '),"\n")
  }
  if(length(x) == 0) {
    xcat(paste("Vecteur", deparse(substitute(x))," vide"))
    return()
  }
  r = as.data.frame(x[!is.na(x)])
  names(r) <- 'valeur'
  xprint(r, title=title, ...)
  invisible()
}

#' @export
out.htest <- function(x, ...) {
  xprint(x)
  ww = attr(x, "warnings")
  if(length(ww) > 0) {
    lapply(ww, function(w) {
      xalert(w$message, type="warning")
    })
  }
}
