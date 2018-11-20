# Handlers
#

#' Define an handler
#' @param name handler name
#' @param func function to call on handler name
output_set_handler <- function(name, func) {
  hh = output_option("handlers")
  hh[[name]] = func
  output_options(handlers=hh)
}

#' Apply Output handlers
#'
#' Call list of registred handlers and pass object
#'
#' @param x object to render
#' @param ... extra parameters
#'
apply_handlers = function(x, ... ) {
  opts = output_option()
  handlers = opts$handlers
  if(is.null(handlers)) {
    return()
  }

  hh = names(handlers)
  for(handler in hh) {
    h = handlers[[handler]]
    if( is.null(h) ) {
      return()
    }
    if(isTRUE(opts$debug)) {
      cat("[output] handler ", handler, "\n")
    }
    h(x, ...)
  }
}

##
# output_file handler
##

#' Default output file handler
#' @param data data to output
#' @param ... extra parameters
output_file <- function(data, ...) {
  UseMethod("output_file")
}

#' Default output_file handler
#' Use the 'name' key in \code{out()}
output_file.default <- function(data, name=NULL, ...) {
  if(is.null(name) || (is.logical(name) & identical(name, FALSE))) {
    return()
  }
  # Caution : out.path is the "out.path" defined in the creation environment of
  # this function .output.file
  # It may not work if another out.path is created in a local environment
  if( !is.data.frame(data) ) {
    d = try(as.data.frame(as.list(data)), silent=T)
    if( "try-error" %in% class(d) ) {
      warning("unable to transform", deparse(substitute(data))," to data.frame")
      return()
    }
    data = d
  }
  write.csv2(data, file=out_path(paste0(name, ".csv")), row.names = FALSE)
}
