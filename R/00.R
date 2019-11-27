#' Output report generation
#' @docType package
#' @name swOutput
#' @author cturbelin
#'
#' This library help to generate report using R.
#' Unlike template based report generator (like Sweave or knitr), there is not template
#' It is inspired from SAS ODS (Output Delivery System) : the program is the template itself.
#'
#' When you want to send an object to the output, you just have to use the out(object) function.
#' The object will be printed using the current output driver
#'
#' As it is a program, the output can adapt itself to add a table or a graph depending on the data or
#' the context.
#' Template based report generator are made to make document (somethings camera-ready), this library is more
#' designed to produce analysis report
#'
#' An output driver is choosen on the output start.
#' \describe{
#'  \item{console}{This make all output to console, useful during dev}
#'  \item{html}{Make html report, based on R2HTML}
#'  \item{pander}{pander output driver}
#' }
#'
#'
NULL # do not remove this null

.config = new.env(parent=emptyenv())

#' Return the absolute path of a file inside the output path
#' @param ... relative path inside the output path
out_path = function(...) {
  paste0(.config$path, ...)
}

#' Get package option from name
#' From 'swOutput' options() entry
#' @param name option name to get,
#' @return one value if name is provided, all if not
#' @seealso output_options
output_option = function(name=NULL) {
  o = getOption("swOutput")
  if(is.null(name)) {
    o
  } else {
    o[[name]]
  }
}


#' SwOutput Options
#'
#' Define output options
#'
#' @export
#'
#' @param ... list of options to replace (at 1 level)
#'
#' @details Main options (1st level):
#' \describe{
#'  \item{handlers}{list of handlers type=func}
#'  \item{default.level}{default level of title}
#'  \item{plugins_path}{path of plugins}
#'  \item{html}{html driver options}
#'  \item{pander}{pander driver options}
#'  \item{inline_css}{add inline css deprecated}
#' }
#'
#' @details HTML options:
#' \describe{
#'  \item{css}{css file path}
#'  \item{theme}{theme name if css not specified}
#'  \item{css.extra}{extra css files}
#'  \item{header}{header content or function returning header content}
#' }
#'
#' @details Pander Options:
#' \describe{
#'  \item{formats}{vector of output formats}
#' }
#'
#'
output_options = function(...) {
  opts = list(...)
  oo = getOption("swOutput")
  if( is.null(oo) ) {
    oo = list()
  }
  if(length(opts) == 0) {
    return(oo)
  }
  nn = names(opts)
  # No names, then list of opts to return
  if( is.null(nn) ) {
    if(length(opts) == 1) {
      return(oo[[unlist(opts)]])
    }
    return(oo[unlist(opts)])
  }
  # Value with names, replace
  for(i in seq_along(opts)) {
    n = nn[i]
    o = opts[[i]]
    oo[[n]] <- o
  }
  base::options("swOutput"=oo)
}

#' Is debug activated
#' @noRd
is_debug <- function() {
  isTRUE(output_option('debug'))
}

#' Deprecated
#' @noRd
safe.cat <- cat

#' @noRd
package_data_file = function(file) {
  system.file("extdata", file, package = "swOutput")
}

#' Call a driver specific function by generic name
#' @param .func_name name of the function to call
#' @param ... parameters to pass to function
call_driver_function = function(.func_name, ...) {
  if( is.null(.config$driver) ) {
    stop('No driver initialized')
  }
  driver_func = paste0(.func_name,"_", .config$driver)
  args = alist(...)
  do.call(driver_func, args)
}
