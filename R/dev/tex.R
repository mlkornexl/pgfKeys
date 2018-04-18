#' Parsing (La)TeX Strings
#'
#' The function parses special characters of a string, so it can be used with
#' (La)TeX. Special characters supported are the underscore (\code{_}),
#' the dollar sign (\code{$}), and the hash sign (\code{#}).
#' 
#' @param x string that shall be converted to a (La)TeX compatible string
#' 
#' @details The string is parsed by escaping the special character with a
#' backslash. If a special character already \emph{is} escaped, nothing will
#' happen.
#' 
#' @export
#' 

parseTeX <- function(x, ...) {
  x <- as.character(x)
  
  # replace underscores
  x <- gsub('([^\\\\]|^)_', '\\1\\\\_', x)
  
  # replace dollar-sign
  x <- gsub('(^|[^\\\\])\\$', '\\1\\\\$', x)
  
  # replace hash-sign
  x <- gsub('(^|[^\\\\])\\#', '\\1\\\\#', x)
  
  x[is.na(x)] <- ''
  
  return(x)
}
