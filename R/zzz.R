.onLoad <- function(libname, pkgname) {
  defaults = list(
    default.level =  4
  )
  do.call(output_options, defaults)
  invisible()
}
