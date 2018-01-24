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


#' Get package options
#' From 'redis.progress' options() entry
get_option = function(name=NULL) {
  o = getOption("swOutput")
  if(is.null(name)) {
    o
  } else {
    o[[name]]
  }
}
