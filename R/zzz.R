.onLoad <- function(libname, pkgname) {
  defaults = list(
    default.level =  4,
    handlers = list(
      file= output_file
    ),
    pander=list(
      formats=c('html')
    )
  )
  do.call(output_options, defaults)
  invisible()
}
