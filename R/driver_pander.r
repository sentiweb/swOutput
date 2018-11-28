#'
#' pander Driver
#'

output_open_pander = function(filename, title) {
  library(pander)

  bin = panderOptions("pandoc.binary")

  if(bin == "") {
      path = Sys.getenv("RSTUDIO_PANDOC")
      if(path != "") {
        path = normalizePath(path)
        if(!file.exists(path)) {
          warning("Provided path in RSTUDIO_PANDOC doesnt exist")
        }
        p = Sys.getenv("PATH")
        p = paste0(p, if(substr(p, nchar(p), nchar(p)) != .Platform$path.sep) .Platform$path.sep else "", path)
        Sys.setenv(PATH=p)
      }

      path = Sys.which("pandoc")
      if(path != "") {
        if( file.exists(path) ) {
          panderOptions("pandoc.binary", path)
        } else {
          warning(paste("pandoc path '", path,"' doesnt exist"))
        }
      }
  }

  fn = paste0(.config$path, filename, ".md")

  if(file.exists(fn)) {
      file.remove(fn)
  }
  .config$report.file = fn
  .config$report.level = 2
}

output_done_pander <- function() {
  opts = output_option('pander')

  working.dir = getwd()

  on.exit(setwd(working.dir))

  formats = opts$formats

  report.file = .config$report.file

  dir = dirname(report.file)
  fn = basename(report.file)

  for(format in formats) {
    # cat("Converting to ", format)
    setwd(dir)
    # cat("before:", getwd())
    try(Pandoc.convert(fn, format=format))
    # cat("after:", getwd())
    setwd(working.dir)
    # cat("end:", getwd())
  }

}

write_to_pander = function(o) {
  cat(paste(o, collapse = "\n"), file=.config$report.file, append = TRUE)
}

wrap_nl = function(...) {
  paste0("\n", ...,"\n")
}

#' xprint print to the output
#' @method xprint_pander default
xprint_pander <- function(x, title="",...) {
    UseMethod("xprint_pander")
}

xprint_pander.default = function(x, ...) {
    args = list(...)
    if( !is.null(args$title) )  {
        xcat(args$title)
    }
    o = pander_return(x)
    write_to_pander(wrap_nl(o))
}

#' @method xprint_pander data.frame
xprint_pander.data.frame=function(x, title="", row.names=F, last.row=F, ...) {
    debug.output = is_debug()
    if(debug.output) {
        cat("xprint.data.frame\n")
    }
    if( nrow(x)==0 ) {
        xcat(title)
        xcat("Tableau vide")
    } else {
      emphasize.strong.rows = NULL
      if(last.row) {
        emphasize.strong.rows = nrow(x)
      }
      write_to_pander(pandoc.table.return(x, row.names=row.names, caption=title, justify = "center", emphasize.strong.rows=emphasize.strong.rows))
    }
}


# xcat
# cat() wrapper
# Utilisez cette fonction au lieu de cat()
xcat_pander = function(...,ln=T) {
  o = paste0(...)
  write_to_pander(pandoc.p.return(o))
}

# Affiche un titre
xtitle_pander = function(..., level=2) {
  o = paste0(...)
  if(length(o) > 0 && o != "") {
    write_to_pander(wrap_nl(pandoc.header.return(o, level=level)))
  }
}

xbloc_pander = function(..., style=NULL, end=F, level=NULL) {
  level = .config$report.level
  if( isTRUE(end) ) {
    if(level > 2) {
      level = level - 1
    }
  } else {
    level = level + 1
  }
  .config$report.level <- level
  if( !end ) {
    xtitle_pander(..., level=level)
  }
}

xheader_pander <- function(...) {}
xheader_end_pander <- function(...) {}
xcomment_pander <- function(...) {}

# add a link

xlink_pander = function(filename, text, attr=NULL) {
  if( is(filename, "connection") ) {

  }
  write_to_pander(pandoc.link.return(filename))
}

xalert_pander = function(x, type="warning", ...) {
  write_to_pander(pandoc.strong.return(x))
}

output_graph_pander = function(file, name=NULL) {
  write_to_pander(wrap_nl(pandoc.image.return(file, caption = name)))
}
