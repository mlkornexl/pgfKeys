#' @include pgfKeyValue-class.R
NULL

#' LaTeX-Command to Set pgf-Keys
#' 
#' @param x a list or an object of type \code{pgfKeyValue}
#' @param absolute logical, see details
#' 
#' @details The parameter \code{absolute} controls the handling of the key's
#'  path. If \code{absolute = TRUE} the key's path is set to an absolute path,
#'  i.e. a slash is prepended (if not already present). In any case, multiple
#'  leading slashes are converted to a single one.
#'  
#'  If \code{absolute = FALSE} absolute paths are converted to relative ones,
#'  i.e. any leading slashes are omitted.
#'  
#'  If \code{absolute} is missing, no changes regarding the path are made.
#' 
#' @exportMethod pgfKeysSetValue
#' @docType methods
#' 
setGeneric(name = 'pgfKeysSetValue',
           def = function(x, absolute = FALSE, ...) {
             standardGeneric('pgfKeysSetValue')
           })

#' @rdname pgfKeysSetValue
setMethod(f = 'pgfKeysSetValue',
          signature = 'list',
          definition = function(x, absolute, ...) {
            x_key <- names(x)
            if (!missing(absolute)) {
              if (absolute) {
                x_key <- sub('^/+', '/', paste0('/', x_key))
              } else {
                x_key <- sub('^/+', '', x_key)
              }
            }
            x_val <- parseTeX(x)
            
            x_cmd <- paste0('\\pgfkeyssetvalue{', x_key, '}{', x_val, '}\n')
            
            return(x_cmd)
          })
