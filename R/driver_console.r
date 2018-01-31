#' Console output driver

#' Open output console driver
output_open.console <- function(filename, titre) {}

#' Close output console driver
output_done.console <- function() {
}

# Output function
# affiche une variable x
xprint_console <- function(x, ...) {
  args = list(...)
  if(!is.null(args$title))  {
    xcat(args$title)
  }
  print(x)
}

# xcat
# cat() wrapper
# Utilisez cette fonction au lieu de cat()
xcat_console <- function(...,ln=T) {
  cat(...)
  if(ln) {
    cat("\n")
  }
}

#' @rdname xtitle
xtitle_console <- function(..., level=2) {
  cat(...,"\n")
}

#' @rdname xbloc
xbloc_console <- function(..., end=F) {
 xcat("----")
}

#' @rdname xheader
xheader_console <- function() {}

xheader.end_console <- function() {}

#' add a link
#' @rdname
xlink_console = function(filename, text, attr=NULL) {}
