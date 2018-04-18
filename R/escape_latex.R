
#' Escape LaTeX Strings
#' 
#' The functions escapes characters that are used as LaTeX control sequences.
#' 
#' @param x a string to be escaped
#' @param newlines if \code{TRUE} new line characters are escaped as a double
#'  backslash
#' @param spaces if \code{TRUE} multiple spaces are escaped to multiple
#'  \emph{protected} whitespaces in LaTeX
#' 
#' @details If \code{x} is of class \code{TeXcontent}, the input string is
#'  returned ``as is''.
#' 
#' @return an string with LaTeX control characters escaped
#' 
#' @references The function was taken from
#' 
#' Yihui Xie (2016). knitr: A General-Purpose Package for Dynamic Report
#' Generation in R. R
#' 
#' @export
escape_latex <- function(x, newlines = FALSE, spaces = FALSE) {
  
  if ('TeXcontent' %in% class(x)) return(x)
  
  x <- gsub('\\\\', '\\\\textbackslash{}', x)
  x <- gsub('([#$%&_{}])', '\\\\\\1', x)
  x <- gsub('~', '\\\\textasciitilde{}', x)
  x <- gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x <- gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x <- gsub('  ', '\\\\ \\\\ ', x)
  return(x)
}