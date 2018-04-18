#' @importFrom knitr knit_print
NULL

#' Printing functions for \code{knitr}
#' 
#' The \code{knit_print} functions fot \code{\link{pgfvalue}} and
#' \code{\link{pgfkeyvalue}} return LaTeX-code to set key-value pairs using
#' LaTeX's \code{pgfkeys}-package.
#' 
#' @param x an object of S3 class \code{\link{pgfvalue}} or
#'  \code{\link{pgfkeyvalue}}
#' @param options a string, a common path for all keys, change_dir if \code{TRUE} the path is set using the \code{/.cd}
#'  handler of the LaTeX command
#' 
#' @details For objects \code{x} of S3 class \code{pgfkeyvalue}
#'  \code{\link[knitr]{knit_print}} returns LaTeX code to set pgf key-value
#'  pairs using \code{\\pgfkeys{key = value}}.
#'  
#'  The optional argument \code{option} contain knitr chunk options. Available
#'  options for printing key value pairs are
#'  \describe{
#'    \item{\code{path}}{a common path to prepend pgf-keys}
#'    \item{\code{change_dir}}{logical, for \code{pgfkeyvalue} objects only;
#'      If \code{TRUE} the path is set globally by using \code{/.cd} handler}
#'    \item{\code{rm_unnamed}}{logical, for \code{pgfvalue} objects only;
#'      If \code{TRUE} unnamed arguments are omitted silently. If \code{FALSE}
#'      an error is thrown on any unnamed argument (default).}
#'  }
#' 
#' @rdname knit_print
#' @method knit_print pgfkeyvalue
#' @export
knit_print.pgfkeyvalue <- function(x, options = list()) {
  
  if (!is.null(options$path)) {
    options$path <- sub('^[[:space:]]+', '', options$path)
    options$path <- sub('[[:space:]]+$', '', options$path)
    options$path <- sub('/+$', '/', paste0(options$path, '/'))
    if (options$path == '/') options$change_dir <- FALSE
  }
  
  if (is.null(options$change_dir)) options$change_dir <- FALSE
  
  if (!is.null(names(x))) {
    i_named <- names(x) != ''
    i_mask <- grepl('[=,]', x) & (!grepl('^\\{.*?\\}$', x))
    
    if (!is.null(options$path) & !options$change_dir) {
      names(x)[i_named] <- paste0(options$path, names(x)[i_named])
    }
    
    x[i_named & i_mask] <- paste0('{', x[i_named & i_mask], '}')
    x[i_named] <- paste(names(x)[i_named], x[i_named], sep = ' = ')
  } else {
    x <- paste0(options$path, x)
  }
  
  x <- paste(x, collapse = ',\t\n')
  if (!is.null(options$path) & options$change_dir) {
    x <- paste0(options$path, '.cd,\t\n', x)
  }
  x <- paste0('\\pgfkeys{', x, '}')
  
  knitr::asis_output(paste0(x, '\n'))
}

#' @rdname knit_print
#' 
#' @details For objects \code{x} of S3 class \code{pgfkeyvalue}
#'  \code{\link[knitr]{knit_print}} returns LaTeX code to set pgf key-value
#'  pairs using \code{\\pgfkeyssetvalue{key}{value}}.
#' 
#' @method knit_print pgfvalue
#' @export
knit_print.pgfvalue <- function(x, options = list()) {

  if (!is.null(options$path)) {
    options$path <- sub('^[[:space:]]+', '', options$path)
    options$path <- sub('[[:space:]]+$', '', options$path)
    options$path <- sub('/+$', '/', paste0(options$path, '/'))
  }
  
  stopifnot(!is.null(names(x)))
  if (is.null(options$rm_unnamed)) options$rm_unnamed <- FALSE
  if (options$rm_unnamed) {
    x <- x[names(x) != ''] 
  } else {
    stopifnot(all(names(x) != ''))
  }
  
  x <- paste0('\\pgfkeyssetvalue{', options$path, names(x), '}{', x, '}')
  x <- paste(x, collapse = '\n')
  
  knitr::asis_output(paste0(x, '\n'))
}