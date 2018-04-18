#' @include pgfKeyValue-class.R
NULL

#' Get Value of pgf-Key
#' 
#' @param x an object of type \code{pgfKeyValue} or a character vector
#' @param na_value string, return value of missing values
#' @param ... additional parameter, not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfValue
#' 
setGeneric(name = 'pgfValue',
           def = function(x, na_value = NA_character_, ...) {
             standardGeneric('pgfValue')
           })

#' @describeIn pgfValue The function returns the \code{value} slot of object
#'  \code{x}.
setMethod(f = 'pgfValue',
          signature = 'pgfKeyValue',
          definition = function(x, na_value, ...) {
            
            x_masked <- gsub('\\\\(\\{|\\})', '--', x@value)
            x_masked <- .maskParantheses(x_masked, '{', '}')
            i_mask <- (!grepl('^\\{.*\\}$', x_masked)) &
              (grepl('^[[:space:]]', x_masked) |
                 grepl('[[:space:]]$', x_masked) |
                 grepl('[,=]', x_masked))
            
            x@value[i_mask] <- paste0('{', x@value[i_mask], '}')
            
            x@value <- rep(x@value, length.out = length(x@key))
            x@value[is.na(x@value)] <- na_value
            
            return(x@value)
          })

#' @describeIn pgfValue The function returns the value part of the character
#'  strings \code{x}.
setMethod(f = 'pgfValue',
          signature = 'character',
          definition = function(x, na_value, mask, ...) {
            x <- .trimString(x, na_value = NA_character_)
            
            i_value <- vapply(pgfKey(x, na_value = '', long_key = TRUE),
                              nchar, FUN.VALUE = integer(1))
            
            x <- Map(function(x, i_value, na_value) {
              if (i_value == nchar(x)) {
                x <- na_value
              } else {
                x <- substr(x, i_value + 1, nchar(x))
                x <- sub('^[[:space:]]*=[[:space:]]*', '', x)
                x <- sub('^\\{(.*)\\}$', '\\1', x)
              }
              return(x)
            }, x = x, i_value = i_value, na_value = na_value)
            
            return(unlist(x, use.names = FALSE))
          })

#' Get Key Name of a pgf-Key
#' 
#' @param x an object of type \code{pgfKeyValue} or a character vector
#' @param na_value string, return value of missing values
#' @param include_path logical
#' @param include_handler logical
#' @param ... additional parameter, not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfKey
#' 
setGeneric(name = 'pgfKey',
           def = function(x, na_value = NA_character_, include_path = TRUE,
                          include_handler = include_path, ...) {
             standardGeneric('pgfKey')
           })

#' @describeIn pgfKey The function returns the \code{key} slot of object
#'  \code{x} (optionally including path and handler).
setMethod(f = 'pgfKey',
          signature = 'pgfKeyValue',
          definition = function(x, na_value, include_path,
                                include_handler, ...) {
            
            i <- !is.na(x@key)
            
            if (include_path) {
              x@key[i] <- paste0(pgfPath(x, na_value = '')[i], x@key[i])
            }
            
            if (include_handler) {
              x@key[i] <- paste0(x@key[i], pgfHandler(x, na_value = '')[i])
            }
            
            x@key[!i] <- na_value
            
            return(x@key)
          })

#' @describeIn pgfKey The function returns the key name part of the character
#'  strings \code{x}.
setMethod(f = 'pgfKey',
          signature = 'character',
          definition = function(x, na_value, include_path,
                                include_handler, ...) {
            
            x <- .trimString(x, na_value = NA_character_)
            
            x_masked <- gsub('\\\\(\\{|\\})', '--', x)
            x_masked <- .maskParantheses(x_masked, '{', '}')
            
            x <- Map(function(x, i_assign, na_value) {
              if (i_assign > 0) x <- substr(x, 1, i_assign - 1)
              return(x)
            }, x = x, i_assign = regexpr('=', x_masked), na_value = na_value)
            
            if (!include_handler) {
              x <- unlist(x)
              i_handler <- regexpr('/.', x_masked, fixed = TRUE)
              i_handler[i_handler <= 0] <- nchar(x)[i_handler <= 0] + 1
              
              x <- Map(substr, x = x, start = 1, stop = i_handler - 1)
              x[i_handler == 1] <- na_value
            }
            
            if (!include_path) {
              x <- unlist(x)
              n_x <- nchar(x)
              x_masked <- Map(substr, x = x_masked, start = 1, stop = n_x)
              i_path <- vapply(gregexpr('(/[^\\.]|/$)', x_masked), max,
                               FUN.VALUE = integer(1))
              
              x <- Map(substr, x = x, start = i_path + 1, stop = n_x)
              x[i_path == n_x] <- na_value
            }
            
            x <- unlist(x, use.names = FALSE)
            x[is.na(x)] <- na_value
            
            return(x)
          })
#' @describeIn pgfKey For a list \code{x}, the names of the list are treated as
#'  as keys.
setMethod(f = 'pgfKey',
          signature = 'list',
          definition = function(x, na_value, include_path,
                                include_handler, ...) {
            return(pgfKey(names(x), na_value = na_value,
                          include_path = include_path,
                          include_handler = include_handler))
          })

#' Get Path of pgf-Key
#' 
#' @param x an object of type \code{pgfKeyValue} or a character vector
#' @param na_value string, return value of missing values
#' @param ... additional parameter, not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfPath
#' 
setGeneric(name = 'pgfPath',
           def = function(x, na_value = NA_character_, ...) {
             standardGeneric('pgfPath')
           })

#' @describeIn pgfValue The function returns the \code{path} slot of object
#'  \code{x} appended by trailing slash.
setMethod(f = 'pgfPath',
          signature = 'pgfKeyValue',
          definition = function(x, na_value, ...) {
            
            x@path <- sub('([^/])$', '\\1/', x@path)
            x@path <- rep(x@path, length.out = length(x@key))
            x@path[is.na(x@path)] <- na_value
            
            return(x@path)
          })

#' @describeIn pgfKey The function returns the path part of the character
#'  strings \code{x}.
setMethod(f = 'pgfPath',
          signature = 'character',
          definition = function(x, na_value, include_family, ...) {
            x <- pgfKey(x, na_value = '', include_path = TRUE,
                        include_handler = FALSE)
            
            x_masked <- gsub('\\\\(\\{|\\})', '--', x)
            x_masked <- .maskParantheses(x_masked, '{', '}')
            
            i_path <- vapply(gregexpr('/', x_masked), max, integer(1))
            
            x <- Map(substr, x = x, start = 1, stop = i_path)
            x[i_path < 0] <- na_value
            x[is.na(x)] <- na_value
            
            return(unlist(x, use.names = FALSE))
          })

#' Get Handler of pgf-Key
#' 
#' @param x an object of type \code{pgfKeyValue} or a character vector
#' @param na_value string, return value of missing values
#' @param ... additional parameter, not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfHandler
#' 
setGeneric(name = 'pgfHandler',
           def = function(x, na_value = NA_character_, ...) {
             standardGeneric('pgfHandler')
           })

#' @describeIn pgfValue The function returns the \code{handler} slot of object
#'  \code{x} prepended by a leading handler identifier (\code{/.}).
setMethod(f = 'pgfHandler',
          signature = 'pgfKeyValue',
          definition = function(x, na_value, ...) {
            x@handler[!is.na(x@handler)] <-
              paste0('/.', x@handler[!is.na(x@handler)])
            x@handler[is.na(x@handler)] <- ''
            
            x@handler <- rep(x@handler, length.out = length(x@key))
            
            return(x@handler)
          })

#' @describeIn pgfKey The function returns the handler part of the character
#'  strings \code{x}.
setMethod(f = 'pgfHandler',
          signature = 'character',
          definition = function(x, na_value, ...) {
            x <- pgfKey(x, na_value = '', include_path = FALSE,
                        include_handler = TRUE)
            
            x_masked <- gsub('\\\\(\\{|\\})', '--', x)
            x_masked <- .maskParantheses(x_masked, '{', '}')
            
            n_x <- nchar(x)
            i_handler <- regexpr('/.', x_masked)
            i_handler[i_handler < 0] <- n_x[i_handler < 0] + 1
            
            x <- Map(substr, x = x, start = i_handler, stop = n_x)
            x[i_handler > n_x] <- na_value
            x[is.na(x)] <- na_value
            
            return(unlist(x, use.names = FALSE))
          })
