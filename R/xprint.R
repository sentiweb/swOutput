
#' Primitive Output object using current driver
#' @export
xprint <- function(...) {
  call_driver_function("xprint", ...)
}

#' Output a link
#' @export
xlink <- function(...) {
  call_driver_function("xlink", ...)
}

#' Output an alert
#' @param x alert message
#' @param type type of warning message warning|info|danger
#' @export
xalert <- function(x, type="warning", ...) {
  call_driver_function("xalert", x=x, type=type, ...)
}

#' Output an comment
#' @param ... strings to add as a comment
#' @export
xcomment <- function(...) {
  call_driver_function("xcomment", ...)
}

#' Output a string
#' @export
xcat =  function(..., style=NULL, ln=T) {
  call_driver_function("xcat", style=style, ln=ln, ...)
}

#' Output a title
#' @export
xtitle = function(..., level=NULL) {
  call_driver_function("xtitle", ..., level=level)
}

#' Output block/section
#' @export
#' @param title title of block
#' @param level level of block
#' @param end if TRUE declare end of current block
xbloc = function(title=NULL, level=F, end=FALSE, ...) {
  call_driver_function("xbloc", ..., level=level, end=end)
}

#' End a block
#' @export
xbloc_end <- function() {
  call_driver_function("xbloc", end=TRUE)
}


