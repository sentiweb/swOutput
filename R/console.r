# Bilan annuel du reseau Sentinelles
# Fonctions de calcul des incidences
# $Id$

.output.open <- function(filename, titre) {}

.output.done <- function() {}

# Output function
# affiche une variable x
xprint <- function(x, ...) {
  args = list(...)
  if(!is.null(args$title))  {
    xcat(args$title)
  }
  print(x)
}

# xcat
# cat() wrapper
# Utilisez cette fonction au lieu de cat()
xcat <- function(...,ln=T) {
  cat(...)
  if(ln) {
    cat("\n")
  }
}

# Affiche un titre
xtitle <- function(...,level=2) {
  cat(...,"\n")
}

xbloc <- function(..., end=F) {
 xcat("----")
}

xheader <- function() {}

xheader.end <- function() {}

# add a link
xlink = function(filename, text, attr=NULL) {}
