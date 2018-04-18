#' Set Output Hooks for knitr
#' 
#' The function \code{render_pgf} can be used to set up output hooks for the
#' use with knitr. It is based on the \code{\link[knitr]{render_latex}()}
#' function of the knitr package and should therefore only be used with LaTeX
#' output format.
#' 
#' @section Default Chunk Options:
#'  The function \code{render_pgf} sets the chunk options \code{error = FALSE}
#'  and \code{message = FALSE} to prevent document generation with erroneous
#'  code.
#' 
#' @section Output Hook Function:
#'  The hook function \code{output} is extended by an additional boolean option
#'  \code{skip_lines}. If \code{TRUE} double line skips are removed and the
#'  line skip at the end of the output is removed to. This is used for the
#'  output of tables (which must not have an empty line at the end).
#' 
#' @section Option Templates:
#'  An option template \code{pgfkey} is defined. It can be used to output
#'  return of \code{\link{pgfvalue}} or \code{\link{pgfkeyvalue}}.
#' 
#' @section Hook for Table Output:
#'  The output hook-function \code{pgfplotstable} can be used to set input for
#'  tables using the LaTeX-command \code{\\pgfplotstableread}. The argument to
#'  the hook is the name of the LaTeX command the table will be stored in.
#'
#' @section Sample Code Chunk:
#'  The following code chunk can be used at the beginning of the document to
#'  initialize pgf-rendering.
#'  
#'  \preformatted{<<setup, include = FALSE>>=
#'  render_pgf()
#'  @@}
#' 
#' @seealso \code{\link{pgfvalue}}, \code{\link{pgfkeyvalue}} for a definition
#'  of the respective S3 classes.
#'  
#'  \code{\link[=knit_print.pgfvalue]{knit_print}} for printing method for
#'  \code{\link{pgfvalue}} and \code{\link{pgfkeyvalue}} class objects.
#' 
#' @export
render_pgf <- function() {
  knitr::render_latex()
  .output_hook <- knitr::knit_hooks$get('output')
  
  knitr::opts_chunk$set(error = FALSE, message = FALSE)
  
  knitr::knit_hooks$set(output = function(x, options) {
    if (is.null(options$skip_lines)) options$skip_lines <- FALSE
    
    if (options$skip_lines) {
      x <- gsub('\\n{2,}', '\n', x)
      x <- sub('\\n$', '', x)
    }
    
    .output_hook(x, options)
    
  })
  
  knitr::opts_template$set(
    pgfkey = list(echo       = FALSE,
                  results    = 'asis',
                  render     = pgf_print,
                  colSep     = '&',
                  rowSep     = '\\\\',
                  path       = '/',
                  setValue   = TRUE,
                  rm_unnamed = FALSE)
  )
  
  knitr::knit_hooks$set(pgfplotstable = function(before, options, envir) {
    if (before) {
      '\\end{kframe}\\pgfplotstableread[col sep = &, row sep = \\\\]{\n\\begin{kframe}'
    } else {
      paste0('\\end{kframe}}\\', options$pgfplotstable, '\\begin{kframe}')
    }
  })
  
  
}
