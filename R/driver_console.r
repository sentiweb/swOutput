#' Console output driver
#'
#' Output object to console using textual representation
#'
#' @name console
NULL

#' Open output console driver
#' @rdname output_open
output_open_console <- function(filename, title) {}

#' Close output console driver
#' @rdname output_open
output_done_console <- function() {
}

#' Output function for console driver
#' affiche une variable x
#' @rdname console
xprint_console <- function(x, ...) {
  args = list(...)
  if(!is.null(args$title))  {
    xcat(args$title)
  }
  print(x)
}

xalert_console <- function(...) {

}

#' xcat
#' cat() wrapper
#' @rdname console
xcat_console <- function(...,ln=T) {
  cat(...)
  if(ln) {
    cat("\n")
  }
}

#' @rdname console
xtitle_console <- function(..., level=2) {
  cat(...,"\n")
}

#' @rdname console
xbloc_console <- function(..., end=F) {
 xcat("----")
}

#' @rdname console
xheader_console <- function() {}

#' @rdname console
xheader.end_console <- function() {}

#' add a link
#' @rdname console
xlink_console = function(filename, text, attr=NULL) {}

output_graph_console = function(...) {
  # Nothing to do
}
