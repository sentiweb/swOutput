

##
# Doc d'utilisation
##
#' Return the absolute path of a file inside the output path
#' @param ... relative path inside the output path
out.path = function(...) {
  paste0(get_option('path'), ...)
}

#' Start the output using a given output driver
#' @param path path of the output
#' @param filename to use (no extension)
#' @param title tile of the file
#' @param type output driver to use
#' @param opts list of options
#' @param plugins list of plugins
#' @param titre (for compatibility, do not use)
init.output <- function(path=getwd(), filename="result", title="", type='console', opts=NULL, plugins=NULL) {
  if( !is.null(titre) ) {
    warning("init.output: 'titre' parameter is deprecated, replace by title")
    title = titre
  }


  if( length(grep("/$",path)) == 0 ) {
    path = paste0(path,'/')
  }

  opts$path <- path
  opts$type <- type

  if( is.null(opts$handlers) ) {
    opts$handlers$file = .output_file
  }

  source(share.path(paste0("lib/output/",type,".r")))
  if( !is.null(plugins) ) {
	lib.path = share.path("lib/output/")
	for(p in plugins) {
		p = gsub("@", lib.path, p) # Allow to refer to share output plugins using "@" prefix
		p = paste0(p,"/",type, ".r")
		if( file.exists(p) ) {
		    source(p)
		}
	}
  }

  dir.create(path, recursive=T, showWarnings=F)

  # Compatibility issues
  xblock <<- xbloc
  share.option(output=opts)
  if( !is.null(filename) ) {
    invisible(.output.open(filename, titre))
  }
}

#' Start a new file (using same output context defined in init.outpu)
start.output <- function(filename, titre="") {
  .output.open(filename,titre)
}

#' Stop the current outpout and close the output file
close.output <- function() {
 .output.done()
}

#' Last graph sent to the outpout
out.lastgraph <- function(titre="") {
}

#' Make a title section
out.title <- function(...) {
 xtitle(...)
}

#'
#' Apply Output handlers
output_handlers = function(x, ... ) {
    opts = share.option('output')
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


#' Main Generic function to output variable
#' Render an object to the ouput
out <- function(x, title=deparse(substitute(x)), ...) {
 if(isTRUE(share.option('output')$debug)) {
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
out.default <- function(x, name=NULL, title="", main="", ... ) {
 title = ifelse(!is.null(main) && main != "", main, ifelse(is.null(title),"",title))

 if( isTRUE(share.option('output')$debug) ) {
   cat("[out.default] ",paste(class(x),collapse=' '),"\n")
   cat("title=",title,"main=",main,"\n")
 }
 xprint(x, title=title, ...)
 invisible()
}

#' out for factors
#' @method out factor
out.factor <- function(x, title="", ...) {
 if(isTRUE(share.option('output')$debug) ) {
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

out.htest <- function(x, ...) {
  xprint(x)
  ww = attr(x, "warnings")
  if(length(ww) > 0) {
    lapply(ww, function(w) {
      xalert(w$message, type="warning")
    })
  }
}

##
# output_file handler
##

#' Default output file handler
output_file <- function(data, ...) {
    UseMethod("output_file")
}

output_file.default <- function(data, name=NULL, ...) {
    if(is.null(name) || (is.logical(name) & identical(name, FALSE))) {
        return()
    }
    # Caution : out.path is the "out.path" defined in the creation environment of
    # this function .output.file
    # It may not work if another out.path is created in a local environment
    if( !is.data.frame(data) ) {
        d = try(as.data.frame(as.list(data)), silent=T)
        if( is.error(d) ) {
            warning("unable to transform", deparse(substitute(data))," to data.frame")
            return()
        }
        data = d
    }
    write.csv2(data, file=out.path(paste0(name, ".csv")), row.names = FALSE)
}

.output_file = output_file