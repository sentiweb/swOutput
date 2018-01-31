
call_driver_function = function(.func_name, ...) {
  if( is.null(.config$driver) ) {
    stop('No driver initialized')
  }
  driver_func = paste0(.func_name,"_", .config$driver)
  call(driver_func, ...)
}

#' Output object using current driver
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
#' @export
xalert <- function(x, type="warning", ...) {
  call_driver_function("xalert", x=x, type=type, ...)
}

#' Output an comment
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

#' Output block
#' @export
xbloc = function(title=NULL, level=F, ...) {
  call_driver_function("xbloc", ..., level=level)
}

xblock <- xbloc

