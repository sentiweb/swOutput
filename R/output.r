
#' Start the output using a given output driver
#' @param path path of the output
#' @param filename to use (no extension)
#' @param title tile of the file
#' @param type output driver to use
#' @param opts list of options
#' @param plugins list of plugins
#' @export
init_output <- function(path=getwd(), filename="result", title="", type='console', opts=NULL, plugins=NULL) {

  if( length(grep("/$",path)) == 0 ) {
    path = paste0(path,'/')
  }

  .config$path <- path
  .config$driver = type

  if( !is.null(plugins) ) {
  	lib.path = output_option('plugins_path')
  	for(p in plugins) {
  	  if( !is.null(lib.path) ) {
  	    # Allow to refer to share output plugins using "@" prefix
  	  	p = gsub("@", lib.path, p)
  	  }
  	  p = paste0(p, "/", type, ".r")
  		if( file.exists(p) ) {
  		    source(p)
  		}
  	}
  }

  dir.create(path, recursive=T, showWarnings=F)

  # Compatibility issues
  # do.call(output_options, opts)

  if( !is.null(filename) ) {
    open_func = paste0("output_open.", type)
    do.call(open_func, list(filename=filename, title=title) )
  }
  invisible()
}

#' @rdname init_output
#' @export
init.output <- init_output

#' Start a new file (using same output context defined in init.output)
#' @export
start_output <- function(filename, titre="") {
  open_func = paste0("output_open.", .config$driver)
  invisible(call(open_func, filename=filename, titre=titre))
}

#' start_output equivalent
#' For compatibilities
#' @export
#' @rdname start_output
start.output <- start_output


#' Stop the current outpout and close the output file
#' @export
close_output <- function() {
  close_func = paste0("output_done.", .config$driver)
  do.call(close_func)
}

#' @rdname close_output
#' @export
close.output <- close_output


#' Last graph sent to the outpout
out.lastgraph <- function(titre="") {
}

#' Make a title section
out_title <- function(...) {
 xtitle(...)
}

#' @rdname out_title
#' @export
out.title <- out_title

#' Main Generic function to output variable
#' Render an object to the ouput
#' @export
out <- function(x, title=deparse(substitute(x)), ...) {
 if( is_debug() ) {
   cat("[out] ",paste(class(x), collapse=' '),"\n")
 }
 output_handlers(x, title=title, ...)
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
