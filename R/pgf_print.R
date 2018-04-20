#' Printing functions for \code{knitr}
#' 
#' The \code{knit_print} functions fot \code{\link{pgfvalue}} and
#' \code{\link{pgfkeyvalue}} return LaTeX-code to set key-value pairs using
#' LaTeX's \code{pgfkeys}-package.
#' 
#' @param x key value pairs to pass to pgf-keys
#' @param options list of current chunk options. For used chunk options, see
#'  the "Chunk Options" section.
#' 
#' @details Chunk Options:
#' \describe{
#'  \item{\code{setValue}}{if \code{TRUE} \code{\\pgfkeyssetvalue} is used
#'    instead of \code{\\pgfkeys}}
#'  \item{\code{path}}{path for pgf-keys}
#'  \item{\code{change_dir}}{if \code{TRUE} \code{.cd}-handler is used instead
#'    of setting path for each key explicitely}
#'  \item{\code{rm_unnamed}}{if \code{TRUE} unname values are omitted, if
#'    \code{FALSE} and names are missing, an error is returned}
#'  \item{\code{colSep}}{delimiter to separate columns in LaTeX tables}
#'  \item{\code{rowSep}}{delimiter to separater rows in LaTeX tables}
#'  \item{\code{tabName}}{name to store read table}
#' }
#' 
#' @rdname pgf_print
#' @export
pgf_print <- function(x, options, ...) UseMethod('pgf_print', x)



#' @rdname pgf_print
#' 
#' @method pgf_print default
#' @export
pgf_print.default <- function(x, options, ...) {
  if (is.null(options$setValue)) options$setValue <- FALSE
  if (options$setValue) x <- pgfvalue(x) else  x <- pgfkeyvalue(x)
  pgf_print(x, options, ...)
}



#' @rdname pgf_print
#' @method pgf_print pgfkeyvalue
#' @export
pgf_print.pgfkeyvalue <- function(x, options, ...) {
  
  if (length(x) == 0) return(knitr::asis_output('\n'))
  
  path <- .pgfPath(options)

  if (!is.null(names(x))) {
    i_named <- names(x) != ''
    i_mask <- grepl('[=,]', x) & (!grepl('^\\{.*?\\}$', x)) & i_named
    
    if (!attr(path, 'change_dir') & (path != '')) {
      names(x)[i_named] <- paste(path, names(x)[i_named], sep = '/')
    }
    
    x[i_mask] <- sprintf('{%s}', x[i_mask])
    x[i_named] <- sprintf('%s = %s', names(x)[i_named], x[i_named])
  } else if (!attr(path, 'change_dir')) {
    x <- paste(path, x, sep = '/')
  }
  
  x <- paste('\t', x, sep = '', collapse = ',\n')
  if (attr(path, 'change_dir')) path <- sprintf('%s/.cd,', path) else path <- ''
  
  x <- sprintf('\n\\pgfkeys{%s\n%s\n}\n', path, x)
  
  knitr::asis_output(x)
}



#' @rdname pgf_print
#' 
#' @details For objects \code{x} of S3 class \code{pgfkeyvalue}
#'  \code{\link[knitr]{knit_print}} returns LaTeX code to set pgf key-value
#'  pairs using \code{\\pgfkeyssetvalue{key}{value}}.
#' 
#' @method pgf_print pgfvalue
#' @export
pgf_print.pgfvalue <- function(x, options, ...) {
  
  if (is.null(options$rm_unnamed)) options$rm_unnamed <- FALSE
  if (is.null(names(x)) & options$rm_unnamed) x <- character(0)
  
  if (length(x) == 0) return(knitr::asis_output('\n'))
  path <- .pgfPath(options)

  stopifnot(!is.null(names(x)))
  if (options$rm_unnamed) {
    x <- x[names(x) != ''] 
  } else {
    stopifnot(all(names(x) != ''))
  }
  
  x <- sprintf('\n\\pgfkeyssetvalue{%s/%s}{%s}\n', path, names(x), x)
  x <- paste(x, collapse = '')
  
  knitr::asis_output(x)
}



#' @rdname pgf_print
#' @method pgf_print data.frame
#' @export
pgf_print.data.frame <- function(x, options, ...) {
  if (is.null(names(x))) names(x) <- paste('x', seq_along(x), sep = '.')
  
  if (is.null(options$colSep)) options$colSep <- '&'
  options$colSep <- sprintf(' %s ', options$colSep)
  
  if (is.null(options$rowSep)) options$rowSep <- '\\\\'
  
  if (is.null(options$tabName)) options$tabName <- 'tab'
  
  x <- vapply(x, methods::as, 'character', FUN.VALUE = character(nrow(x)))
  x <- c(paste(colnames(x), collapse = options$colSep),
         apply(x, 1, paste, collapse = options$colSep))
  x <- paste('\t', x, ' ', options$rowSep, sep = '', collapse = '\n')
  
  x <- paste('', sprintf('\\pgfplotstableread[col sep = %s, row sep = %s]{',
                         options$colSep, options$rowSep),
             x, sprintf('}\\%s', options$tabName), sep = '\n')
  
  knitr::asis_output(x)
}



#' @rdname pgf_print
#' @description \code{.pgfPath} is an auxilliary function to get the pgf-keys'
#'  path.
#' @return The internal function \code{.pgfPath} returns the full path for
#'  pgfkeys with one attribute \code{change_dir}.
#' @keywords internal
.pgfPath <- function(options) {
  if (is.null(options$path)) options$path <- ''
  if (is.null(options$change_dir)) options$change_dir <- FALSE
  
  options$path <- sub('^[[:space:]]+', '', options$path)
  options$path <- sub('[[:space:]/]+$', '', options$path)
  
  if (options$path == '') {
    options$change_dir <- FALSE
  } else if (is.null(options$change_dir)) {
    options$change_dir <- FALSE
  }
  
  structure(options$path, change_dir = options$change_dir)
}
