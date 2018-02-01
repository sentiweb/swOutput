#' Console output driver

#' Open output console driver
#' @rdname output_open
output_open_console <- function(filename, title) {}

#' Close output console driver
#' @rdname output_open
output_done_console <- function() {
}

#' Output function for console driver
#' affiche une variable x
#' @rdname xprint
xprint_console <- function(x, ...) {
  args = list(...)
  if(!is.null(args$title))  {
    xcat(args$title)
  }
  print(x)
}

#' xcat
#' cat() wrapper
#' @rdname xcat
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

xheader_end_console <- function() {}

#' add a link
#' @rdname xlink
xlink_console = function(filename, text, attr=NULL) {}

output_graph_console = function(...) {
  # Nothing to do
}
