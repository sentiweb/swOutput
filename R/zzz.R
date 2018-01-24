.onLoad <- function(libname, pkgname) {

  o = get_option()

  defaults = list(
      default.level =  4
  )

  if( is.null(o) ) {
    o = defaults
  } else {
    for(n in names(defaults)) {
      if(is.null(o[[n]])) {
        o[[n]] = defaults[[n]]
      }
    }
  }
  base::options("swOutput"=o)
  invisible()
}
