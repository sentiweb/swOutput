.onLoad <- function(libname, pkgname) {
  defaults = list(
    default.level =  4,
    handlers = list(
      file= output_file
    )
  )
  do.call(output_options, defaults)
  invisible()
}
