#' Sentiweb Output Library
#' HTML Output

#' @noRd
output_open.html = function(filename, title) {
  library(R2HTML)
  options(R2HTML.format.digits=10)

 opts = output_option()

 css = opts$html$css

 if( is.null(css) ) {
    theme = ifelse( is.null(opts$html$theme), "serious", opts$html$theme )
    css = file(package_data_file(paste0(theme,'.css')))
    do.copy = T
    inline_css = ifelse(!is.null(opts$inline.css), opts$inline.css, TRUE)
 } else {
   do.copy = F
   inline_css = F
 }

 if( inline_css ) {
  css_tag = paste(readLines(css), collapse="\n")
  close(css)
  if( !is.null(opts$html$css.extra) ) {
    extra = sapply(opts$html$css.extra, function(file) {
        paste(readLines(file), collapse = "\n")
    })
    extra = paste(extra, collapse = "\n")
    css_tag = paste(css_tag, extra)
  }
  css_tag = paste0('<style>', css_tag, '</style>')
 } else {
   if(do.copy) {
     target.css = paste(.config$path, basename(css))
     if( !file.exists(target.css) | ( file.info(target.css)$mtime < file.info(css)$mtime)  ) {
       file.copy(css , target.css,  overwrite = T)
     }
     css = basename(css)
   }
   css_tag = paste0('<link rel="stylesheet" href="', css,'" type="text/css">')
 }

 h = opts$html$header
 h = ifelse(is.null(h), "", h)
 h = paste(h, collapse="\n")

 file <- file.path(.config$path, paste0(filename, ".", "html"))

 assign(".HTML.file", file, envir = .GlobalEnv)

 if( exists("HTMLSetFile") ) {
	  HTMLSetFile(file)
 }

 .config$block.level <- NULL

 txt <- paste0("<html>\n<head>\n<title>",title,"</title>\n", css_tag, h,"</head><body>")
 cat(txt, file = file, append = FALSE)
}

#' @noRd
output_done.html <- function() {
 HTMLEndFile()
}

# redefine output graph
#' @noRd
.output.graph = function(file, name) {
  xhtml('<figure class="graph"><img src="',file,'" alt="graph"/><figcaption>',name,'</figcaption></figure>')
}


#' Private functions do not use in external scripts
#' @noRd
xdiv = function(x,...) {

}

#' Add an alert section in the output
#' @rdname xalert
xalert_html = function(x, type="warning", ...) {
  clz = c("alert", paste0("alert-", type))
  xtag("p", value=x, attr=list(class=paste(clz, collapse = " ")))
}

#' Creation d'un tag HTML
#' @param name tag name
#' @param value tag value
#' @param attr attributes of the tag
xtag = function(name, value=NULL, attr=NULL) {
 if( !is.null(attr)) {
   attr = as.list(attr)
   attr = paste0(names(attr), '="', attr, '"')
   attr = paste(" ", paste(attr, collapse=' '))
 } else {
   attr = ""
 }
 if(!is.null(value)) {
  xhtml(paste0('<',name, attr, '>',value,'</',name,'>'))
 } else {
  xhtml(paste0('<',name, attr, '/>'))
 }
}

#' add a link to HTML output
xlink_html = function(filename, text, attr=NULL) {
 if(is.null(attr)) {
   attr = list()
 }
 attr$href = filename
 xtag('a', text, attr)
}

#' Add a comment
xcomment_html <-  function(...) {
  xhtml(paste("<!-- ",...," -->"))
}

#' Output generic html
xhtml <- function(...) {
 safe.cat(..., file=.HTML.file, append=T, sep='')
}

#' xprint print to the output
#' @method xprint default
xprint_html <- function(x, title="",...) {
 UseMethod("xprint_html")
}

# Output function
#' @method xprint character
xprint_html.character <- function(x, title="", class="character", ...) {
  if(is_debug()) {
   cat("xprint.char\n")
  }
 xtag('p', x, list('class'=class))
}

#' @noRd
.xhtml.empty_table <- function(title) {
 xhtml('<table class="table empty-table" align="center"><caption>',title,'</caption><tr><td>Tableau vide</td></tr></table>')
}


#' Output function
#' affiche une variable x
#' @method xprint default
xprint_html.default <-  function(x, title="", ...) {
  if(is_debug()) {
   cat("xprint.default\n")
  }
  xhtml('<div class="default">')
  if(title != "") {
    xtitle(title, level=4)
  }
  HTML(x)
  xhtml('</div>')
}

#' @method xprint matrix
xprint_html.matrix=function(x, title="",...) {
  xprint.data.frame(x, title=title,...)
}

#' @method xprint table
xprint_html.table = function(x, title="", ...) {
  if( is_debug() ) {
   cat("xprint.table\n")
  }
  d = dim(x)
  if( any(d == 0) ) {
	  .xhtml.empty_table(title)
	  invisible(return())
  }

  nn = names(dimnames(x))
  dnn = length(nn)
  has.dimnames = any( nn != "" )

  if(has.dimnames) {
    xhtml('<table class="table" align="center">')
    xhtml('<caption>',title,'</caption>',"\n","<tbody>")
    xhtml(paste0('<tr><td colspan="',dnn,'" align="center">',nn[ ifelse(dnn==1,1,2) ],'</td></tr>'))
    xhtml('<tr>')
    if(dnn > 1) {
      xhtml('<td align="right" valign="top">',nn[1],'</td>')
      xhtml('<td>')
    }
  } else {
    xprint_html(title, class="title")
  }

  if("xtabs" %in% class(x)) {
    HTML(x)
  } else {
    HTML(x, Border=NULL)
  }
  if(has.dimnames) {
    if(dnn > 1) {
      xhtml('</td>')
    }
    xhtml('</tr></tbody></table>')
  }
}
xprint_html.tbl_tf <- function(x, ...) {
  if( is_debug() ) {
    cat("xprint.table\n")
  }
  x = as.data.frame(x)
  xprint_html.data.frame(x, ...)
}

#' @method xprint data.frame
xprint_html.data.frame <- function(x, title="", row.names=F, last.row=F, ...) {
  debug.output = is_debug()
  if(debug.output) {
   cat("xprint.data.frame\n")
  }

  if(is(x, "tbl_df")) {
    x = as.data.frame(x)
    if(debug.output) {
      cat("casting to data.frame\n")
    }
  }

  if( nrow(x) == 0 ) {
   xcat(title)
   xcat("Tableau vide")
  } else {
    xhtml("<div class=\"dataframe\">")
  	if(debug.output) {
  		cat("data.frame.title '",title,"' row.names=",row.names," last-row=",last.row,"\n")
  	}
    class.last.row = ifelse(last.row, "last-row","")
    HTML.data.frame(x, caption=title, Border=NULL, innerBorder=0, captionalign="top", classtable="innerdataframe", row.names=row.names, class.last.row = class.last.row, ...)
    xhtml("</div>")
  }
}

#' @method xprint by
xprint_html.by <- function(x, title="",...) {
 xhtml('<dl class="by-table">')
 nn = names(x)
 name = names(dimnames(x))
 for(i in 1:length(x)) {
   xtag('dt', paste(name,'=', nn[i]))
   xhtml('<dd>')
   xprint_html(x[[i]])
   xhtml('</dd>')
 }
 xhtml('</dl>')
}

#' xcat cat() wrapper for output, like cat but for output
#'
xcat_html =  function(..., style=NULL, ln=T) {
  if( !is.null(style) ) {
   xhtml(paste0('<div class="',style,'">'))
  }
  HTML(paste(...))
  if( !is.null(style) ) {
   xhtml('</div>')
  }
}

#' Render a title section
xtitle_html = function(..., level=NULL) {
 if( is.null(level) ) {
    level = output_option('default.level')
 }
  HTML.title(paste(...),HR=level)
}

#' Add a block section
#' @param title title of the block
#' @param end indicate and en block (title, style are ignored)
#' @param style css class to add to the block
#' @usage
#'  xbloc("my title")
#'  xbloc(end=T)
xbloc_html = function(title=NULL, end=F, style=NULL) {
 block.level = .config$block.level
 if(end) {
  xhtml('</div></div>')
  n = length(block.level)
  if(n > 1) {
	  block.level = block.level[-n]
  }
 } else {
  n = length(block.level)
  if(n > 0) {
	  block.level[ n ] = block.level[ n ] + 1 # increasing upper level count
  }
  block.level = c(block.level, 1) # adding one level
  if( !is.null(title) ) {
    b = paste(block.level[1:n], collapse='.') # list of levels (without last, allways 1)
	tr = paste0('<h2 class="title_bloc"> #',b," ",title,'</h2>')
  } else {
    tr = ''
  }
  if(!is.null(style)) {
    cc = paste(' ', style)
  } else {
    cc = ''
  }
  xhtml('<div class="block',cc,'">',tr,'<div class="block_inner">')
 }
 .config$block.level = block.level
}

#' Add an header
xheader <- function(title=NULL, sub=NULL) {
  xhtml('<div id="head">')
  if( !is.null(title) ) {
    xtitle_html(title, level=1)
  }
  if( !is.null(sub) ) {
    for(sb in sub) {
      xtitle_html(sb, level=2)
    }
  }
}

#' End of the header section
xheader.end <- function() {
  xhtml('</div>')
}



#' Rewrite HTML.data.frame because standard version is ugly
#' @noRd
HTML.data.frame <- function (x, file = get(".HTML.file"), Border = 1, innerBorder = 0,
          classfirstline = "firstline", classfirstcolumn = "firstcolumn",
          classcellinside = "cellinside", append = TRUE, align = "center",
          caption = "", captionalign = "bottom", classcaption = "captiondataframe",
          classtable = "dataframe", digits = getOption("R2HTML.format.digits"),
          nsmall = getOption("R2HTML.format.nsmall"), big.mark = getOption("R2HTML.format.big.mark"),
          big.interval = getOption("R2HTML.format.big.interval"), decimal.mark = getOption("R2HTML.format.decimal.mark"),
          row.names = TRUE, class.last.row = "",
          ...)
{
  if( !is.data.frame(x) ) {
    x = as.data.frame(x)
  }

  cat("\n", file = file, append = append)
  txt = ""
  txtcaption <- ifelse(is.null(caption), "", paste0("\n<caption align=\"", captionalign, "\" class=\"", classcaption, "\">", caption, "</caption>\n"))
  txt <- paste0(txt, "\n<table class=", classtable, ">", txtcaption)
  txt <- paste(txt, "\t<thead>", sep = "\n")
  VecDebut <- c(if (row.names) "\n\t\t<th>", rep("\n\t\t<th>", ncol(x) - 1))
  VecMilieu <- c(if (row.names) "&nbsp;", as.character(dimnames(x)[[2]]))
  VecFin <- c(if (row.names) "</th>", rep("</th>", ncol(x) - 1))
  txt <- paste0(txt, "\n\t<tr class=\"", classfirstline, "\">", paste0(VecDebut, VecMilieu, VecFin, collapse = ""), "\n\t</tr>")
  txt <- paste(txt, "\t</thead><tbody>", sep = "\n")
  x.formatted <- format(x, digits = digits, nsmall = nsmall, big.mark = big.mark, big.interval = big.interval, decimal.mark = decimal.mark)
  x.formatted <- as.matrix(x.formatted)
  x.formatted[is.na(x.formatted)] <- " "
  x.formatted[is.nan(x.formatted)] <- " "
  dim.x = dim(x)
  n.row = dim.x[1]
  n.col = dim.x[2]
  for (i in 1:n.row) {
    if (i == 1) {
      VecDebut <- c(
        if (row.names) paste0("\n<td class=\"", classfirstcolumn, "\">"),
        paste0("\n<td class=\"", classcellinside, "\">"),
        rep(paste0("\n<td class=\"", classcellinside, "\">"), n.col - 1)
      )
      VecMilieu <- c(
          if (row.names) dimnames(x)[[1]][i],
          HTMLReplaceNA(x.formatted[i, ])
      )
      VecFin <- c(
        if (row.names) "\n</td>",
        rep("\n</td>", n.col - 1),
        "\n</td></tr>\n"
      )
    }
    else {
      VecDebut <- c(
        if (row.names) paste0("\n<td class=", classfirstcolumn, ">"),
        paste(rep(paste0("\n<td class=", classcellinside, ">"), n.col))
      )
      VecMilieu <- c(
        if (row.names) dimnames(x)[[1]][i],
        HTMLReplaceNA(x.formatted[i, ])
      )
      VecFin <- c(
        if (row.names) "\n</td>",
        rep("\n</td>", n.col - 1),
        "\n</td></tr>\n"
      )
    }
    tr = paste("\n<tr", ifelse(i == n.row, paste0('class="', class.last.row,'"'),""),">")
    txt <- paste(txt, tr, paste0(VecDebut, VecMilieu, VecFin,  collapse = ""))
  }
  txt <- paste(txt, "\n\t</tbody>\n</table>\n")
  safe.cat(txt, file=file, sep='', append=T)
}

#' Need this function
#' From R2HTML
#' @noRd
"HTMLReplaceNA"<- function(Vec, Replace = " ")
{
	Vec <- as.character(Vec)
	for(i in 1:length(Vec)) {
		try(if((Vec[i] == "NA") | (Vec[i] == "NaN") | is.na(Vec[i])){ Vec[i] <- Replace})
	}
	Vec
}

#' Override Htest
#' @noRd
"HTML.htest" <- function (x, digits = 4, quote = TRUE, prefix = "",file=get(".HTML.file"),append=TRUE, ...)
{
    xbloc(paste("Test: ", x$method), style="htest")
    xhtml('<ul>')
    HTMLli(paste("\n data:<span class=\"dataname\">",x$data.name,"</span>\n",sep=""),file=file,append=TRUE,...)
   out <- character()
    if ( !is.null(x$statistic) )
                out <- c(out, paste(names(x$statistic), "=<b>", format(round(x$statistic,4)),"</b>"))
    if (!is.null(x$parameter))
                out <- c(out, paste(names(x$parameter), "=<b>", format(round(x$parameter,3)),"</b>"))
    if (!is.null(x$p.value))
                out <- c(out, paste("p-value =<font class='pvalue'>", format.pval(x$p.value),"</font>"))
    HTMLli(paste(out,collapse=" , "),file=file,append=TRUE,...)
    if (!is.null(x$alternative)) {
        HTMLli("alternative hypothesis: ",file=file)
        if ( !is.null(x$null.value) ) {
            if (length(x$null.value) == 1) {
               alt.char <- switch(x$alternative, two.sided = "not equal to",
                  less = "less than", greater = "greater than")
                HTMLli(paste("true", names(x$null.value), "is", alt.char,
                 x$null.value, "\n"),file=file,append=TRUE,...)
            }
            else {
               HTMLli(paste(x$alternative, "\nnull values:\n<br>"),file=file,append=TRUE,...)
               HTMLli(x$null.value, file=file,append=TRUE,...)
            }
        }
        else HTMLli(paste(x$alternative, "\n<br>"),file=file,append=TRUE,...)
    }
    if (!is.null(x$conf.int)) {
        HTMLli(paste("<b>",format(100 * attr(x$conf.int, "conf.level")), "</b> percent confidence interval:\n",
         "<b>[", paste(format(c(x$conf.int[1], x$conf.int[2])),sep="",collapse=" ;"),"]</b>",sep=""),file=file,append=TRUE,...)
    }
    xhtml('</ul>')
    if (!is.null(x$estimate)) {
        HTML("sample estimates:\n",file=file,...)
        HTML(t(as.matrix(x$estimate)),file=file,...)
    }
    xbloc(end=T)
    invisible(x)
}

