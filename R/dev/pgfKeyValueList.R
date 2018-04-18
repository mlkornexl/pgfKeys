#' @include pgfKeyValue-class.R
NULL

#' Format Key-Value-List
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param expand_family logical, if \code{TRUE} the family of the keys is
#'  expanded, i.e. it is prepended to each key's path
#' @param new_line logical, if \code{TRUE} the keys are separated by a new line.
#'  Otherwise they are separated by a comma
#' @param mask logical, if \code{TRUE} the returned string is masked as a whole
#'  (i.e. put between curly brackets), even if it would not be necessary
#' @param ... additional parameter, if \code{x} is missing, these paramters are
#'  used to set up a new \code{pgfKeyValue} object.
#' 
#' @docType methods
#' @exportMethod pgfKeyValueList
#' 
setGeneric(name = 'pgfKeyValueList',
           def = function(x, expand_family = FALSE, new_line = FALSE,
                          mask = FALSE, ...) {
             standardGeneric('pgfKeyValueList')
           })

#' @rdname pgfKeyValueList
setMethod(f = 'pgfKeyValueList',
          signature = 'pgfKeyValue',
          definition = function(x, expand_family, new_line, mask, ...) {
            
            i_mask <- grepl('[,=]', x@value) &
              (!grepl('^[[:space:]]*\\{.*\\}[[:space:]]*$', x@value))
            x@value[i_mask] <- paste0('{', x@value[i_mask], '}')
            
            x@path <- sub('/+$', '', x@path)
            x@path[!is.na(x@path)] <- paste0(x@path[!is.na(x@path)], '/')
            x@path[is.na(x@path)] <- ''
            
            x@handler[!is.na(x@handler)] <-
              paste0('/.', x@handler[!is.na(x@handler)])
            x@handler[is.na(x@handler)] <- ''
            
            x@key[!is.na(x@key)] <- paste0(x@path[!is.na(x@key)],
                                           x@key[!is.na(x@key)],
                                           x@handler[!is.na(x@handler)])
            
            i_key_value <- (!is.na(x@key)) & (!is.na(x@value))
            x@key[i_key_value] <- paste(x@key[i_key_value],
                                        x@value[i_key_value], sep = '=')
            x@key[is.na(x@key)] <- x@value[is.na(x@key)]
            
            x@key <- x@key[!is.na(x@key)]
            
            if (length(x@key) <= 0) return('')
            
            if ((!expand_family) & (x@family != '')) {
              x@key <- c(paste0(x@family, '/.cd'), x@key)
            }
            
            x@key <- paste(x@key, collapse = ifelse(new_line, ',\n\t', ', '))
            
            i_mask <- (grep('[,=]', .maskParantheses(x@key, '{', '}'))) | mask
            x@key[i_mask] <- paste0('{', x@key[i_mask], '}')
            
            return(x@key)
          })

#' @describeIn pgfKeyValueList For \code{x} a character, a new
#'  \code{pgfKeyValue}-object is created using \code{x} as value and \code{...}
#'  as the other parameters.
setMethod(f = 'pgfKeyValueList',
          signature = 'character',
          definition = function(x, expand_family, new_line, ...) {
            x <- pgfKeyValue(Key_value = x, ...)
            return(pgfKeyValueList(x, expand_family, new_line))
          })

#' @describeIn pgfKeyValueList For \code{x} missing, a new
#'  \code{pgfKeyValue}-object is created using \code{...} as parameters.
setMethod(f = 'pgfKeyValueList',
          signature = 'missing',
          definition = function(x, expand_family, new_line, ...) {
            x <- pgfKeyValue(...)
            return(pgfKeyValueList(x, expand_family, new_line))
          })

#' Generate a \code{pgfkeys}-Command
#' 
#' This function is used to create a \code{pgfkeys} command to be used with
#' LaTeX.
#' 
#' @param x an object of type \code{pgfKeyValue} or a character
#' @param ... additional parameter, used with \code{x} a character (or missing)
#'  to create a new \code{pgfKeyValue} object
#' 
#' @docType methods
#' @exportMethod pgfKeyValueCommand
#' 
setGeneric(name = 'pgfKeyValueCommand',
           def = function(x, ...) standardGeneric('pgfKeyValueCommand'))

#' @rdname pgfKeyValueCommand
setMethod(f = 'pgfKeyValueCommand',
          signature = 'pgfKeyValue',
          definition = function(x, ...) {
            x_cmd <- pgfKeyValueList(x, expand_family = TRUE, new_line = TRUE,
                                     mask = TRUE)
            x_cmd <- paste0('\\pgfkeys', x_cmd)
            cat(x_cmd)
          })

#' @describeIn pgfKeyValueCommand For \code{x} a character, a new
#'  \code{pgfKeyValue}-object is created using \code{x} as value and \code{...}
#'  as the other parameters.
setMethod(f = 'pgfKeyValueCommand',
          signature = 'character',
          definition = function(x, ...) {
            x <- pgfKeyValue(Key_value = x, ...)
            pgfKeyValueCommand(x)
          })

#' @describeIn pgfKeyValueCommand For \code{x} missing, a new
#'  \code{pgfKeyValue}-object is created using \code{...} as parameters.
setMethod(f = 'pgfKeyValueCommand',
          signature = 'missing',
          definition = function(x, ...) {
            x <- pgfKeyValue(...)
            pgfKeyValueCommand(x)
          })