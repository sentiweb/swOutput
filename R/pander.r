## 
# pander Driver
##

.output.open = function(filename, titre) {
  library(pander)
  
  fn = file.path(share.option('output')$path, paste0(filename, ".", "md"))
  
  if(file.exists(fn)) {
      file.remove(fn)
  }  
  
  .Sentiweb$output.report.file  <<- fn
  .Sentiweb$output.report.level <<- 2
}

.output.done <- function() {
  opts = share.option('output')
  formats = opts$pander$formats
  if( is.null(formats) ) {
      formats = c('html','docx')
  }
  report.file = share.option('output.report.file')
  sapply(formats, function(format) { Pandoc.convert(report.file, format=format) })
}


#' xprint print to the output
#' @method xprint default
xprint <- function(x, title="",...) {
    UseMethod("xprint")
}

xprint.default = function(x, ...) {
    args = list(...)
    if(!is.null(args$title))  {
        xcat(args$title)
    }
    o = pander_return(x)
    .output.write(o)
}

#' @method xprint data.frame
xprint.data.frame=function(x, title="", row.names=F, last.row=F, ...) {
    debug.output = isTRUE(share.option('output')$debug)
    if(debug.output) {
        cat("xprint.data.frame\n")
    }
    if( nrow(x)==0 ) {
        xcat(title)
        xcat("Tableau vide")
    } else {
        .output.write(pandoc.table.return(x, row.names=row.names, caption=title, justify = "center"))
    }
}

.output.write = function(o) {
  cat(paste(o, collapse = "\n"), file=.Sentiweb$output.report.file, append = TRUE)
}

# xcat
# cat() wrapper
# Utilisez cette fonction au lieu de cat()
xcat = function(...,ln=T) {
  o = paste0(...)
  .output.write(pandoc.p.return(o))
}

# Affiche un titre
xtitle = function(...,level=2) {
  o = paste0(...)
  if(length(o) > 0 && o != "") {
    .output.write(pandoc.header.return(o, level=level)) 
  }
}

xbloc = function(..., style=NULL, end=F) {
  level= .Sentiweb$output.report.level
  if( isTRUE(end) ) {
    if(level > 2) {
      level = level - 1
    }
  } else {
    level = level + 1    
  }
  .Sentiweb$output.report.level <<- level
  if( !end ) {
    xtitle(..., level=level)
  }
}

xheader <- function(...) {}
xheader.end <- function(...) {}
xcomment <- function(...) {}

# add a link

xlink = function(filename, text, attr=NULL) {
  if( is(filename, "connection") ) {
    
  }
  .output.write(pandoc.link.return(filename))
}

xalert = function(x, type="warning", ...) {
  .output.write(pandoc.strong.return(x))
}

.output.graph = function(...) {
     # Not handled yet
}
