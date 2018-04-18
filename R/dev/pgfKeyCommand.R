#' @include pgfKeyValue-class.R
NULL

#' LaTeX-Command to Set pgf-Keys
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param family optional common path for pgf-keys
#' 
#' @exportMethod pgfKeyCommand
#' @docType methods
#' 
setGeneric(name = 'pgfKeyCommand',
           def = function(x, family, ...) standardGeneric('pgfKeyCommand'))

#' @rdname pgfKeyCommand
setMethod(f = 'pgfKeyCommand',
          signature = 'pgfKeyValue',
          definition = function(x, family, ...) {
            x_cmd <- as(x, 'character')
            if (!missing(family)) {
              x_cmd <- c(paste(family, '/.cd', sep = ''), x_cmd)
            }
            x_cmd <- x_cmd[!is.na(x_cmd)]
            
            x_cmd <- paste(x_cmd, collapse = ',\n\t')
            x_cmd <- paste0('\\pgfkeys{', x_cmd, '}')
            
            return(x_cmd)
          })

#' @describeIn pgfKeyCommand A list \code{x} is coerce to an object of type
#'  \code{pgfKeyValue}.
setMethod(f = 'pgfKeyCommand',
          signature = 'list',
          definition = function(x, family, ...) {
            x <- pgfKeyValue(key = x)
            if (missing(family)) {
              x_cmd <- pgfKeyCommand(x, ...)
            } else {
              x_cmd <- pgfKeyCommand(x, family, ...)
            }
            return(x_cmd)
          })

#' @describeIn pgfKeyCommand A character \code{x} is coerce to an object of type
#'  \code{pgfKeyValue}.
setMethod(f = 'pgfKeyCommand',
          signature = 'character',
          definition = function(x, family, ...) {
            x <- pgfKeyValue(key = x)
            if (missing(family)) {
              x_cmd <- pgfKeyCommand(x, ...)
            } else {
              x_cmd <- pgfKeyCommand(x, family, ...)
            }
            return(x_cmd)
          })
