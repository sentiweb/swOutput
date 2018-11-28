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
    call_driver_function("output_open", filename=filename, title=title )
  }
  invisible()
}

#' @rdname init_output
#' @export
init.output <- init_output

#' Start a new file (using same output context defined in init.output)
#' @param filename file report name
#' @param titre title of report
#' @export
start_output <- function(filename, titre="") {
  call_driver_function("output_open", filename=filename, title=titre)
}

#' start_output equivalent
#' For compatibilities
#' @export
#' @rdname start_output
start.output <- start_output


#' Stop the current outpout and close the output file
#' @export
close_output <- function() {
  call_driver_function("output_done")
}

#' @rdname close_output
#' @export
close.output <- close_output

#' Make a title section
#' @export
#' @param ... coerced to string before render to report
out_title <- function(...) {
  xtitle(...)
}

#' Output a graph into report
#' @export
#' @param file file
#' @param name name of the graph (caption)
output_graph <- function(file, name=NULL) {
  call_driver_function("output_graph", file=file, name=name)
}

#' @export
#'
out_graph = output_graph


#' @rdname out_title
#' @export
out.title <- out_title
