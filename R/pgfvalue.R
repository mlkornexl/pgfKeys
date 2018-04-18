#' PGF Key-Value Pairs
#' 
#' The methods generate key-value pairs as character strings.
#' 
#' @param x an object that can be coerced to a key-value pair; See signatures
#'  on methods for available object classes.
#' @param ... additional parameters, these can be named or unnamed, see details
#' 
#' @details All arguments (\code{x} as well as additional arguments \code{...})
#'  can be either named, i.e. \code{key = value} or unnamed, i.e. \code{key}.
#'  If an unnamed argument is a named vector (or list) itself it will be
#'  expanded to named arguments.
#'  
#' @return The methods \code{pgfvalue()} return an object of S3 class
#'  \code{pgfvalue}. It contains a character string of key-value pairs. If a
#'  name is given, it is the key's name and the entry is the corresponding
#'  value. If no name is given, the entry contains the key itself.
#' 
#' @export
setGeneric(name = 'pgfvalue',
           def = function(x, ...) standardGeneric('pgfvalue'))



#' @rdname pgfvalue
#' 
#' @details For \code{x} a character any special characters will be escaped,
#'  using \code{\link{escape_latex}}. If \code{x} contains special charcaters
#'  that should remain unescaped (e.g. if value contains LaTeX commands), the
#'  class of \code{x} should be set to \code{"pgfvalue"} explicitely before
#'  calling \code{pgfvalue()}, see Examples.
#' 
#' @examples
#' ## unnamed arguments
#' pgfvalue('key 1', 'key 2')
#' 
#' ## named arguments
#' pgfvalue(key_1 = 'value 1', key_2 = 'value 2')
#' 
#' ## named vector
#' pgfvalue(c(key_1 = 'value 1', key_2 = 'value 2'))
#' 
#' ## escaping special character
#' x <- c(key_1 = 'value_1', key_2 = '\\command{argument}')
#' pgfvalue(x)
#' 
#' class(x) <- 'pgfvalue'
#' pgfvalue(x)
#' 
#' @export
setMethod(f = 'pgfvalue',
          signature = 'character',
          definition = function(x, ...) {
            
            if (length(x) > 1) x <- as.list(x)
            if ('x' %in% names(sys.call())) x <- list(x = x)
            
            if (is.list(x)) {
              x <- pgfvalue(c(x, list(...)))
            } else {
              x <- escape_latex(x)
              if (length(sys.call()) > 2) x <- c(x, pgfvalue(list(...)))
            }
            
            return(structure(x, class = 'pgfvalue'))
          })



#' @rdname pgfvalue
#' @examples
#' ## numeric values
#' pgfvalue(num_1 = 1.3, num_2 = 12.23)
#' 
#' @export
setMethod(f = 'pgfvalue',
          signature = 'numeric',
          definition = function(x, ...) {
            x <- structure(as.character(x), names = names(x))
            if ('x' %in% names(sys.call())) x <- list(x = x)
            pgfvalue(x, ...)
          })



#' @rdname pgfvalue
#' 
#' @details Logical values are given as \code{true} or \code{false}.
#' 
#' @examples
#' ## logical values
#' pgfvalue(bool = TRUE)
#' 
#' @export
setMethod(f = 'pgfvalue',
          signature = 'logical',
          definition = function(x, ...) {
            if (is.na(x)) {
              x <- NA_character_
            } else {
              x <- ifelse(x, 'true', 'false')
            }
            if ('x' %in% names(sys.call())) x <- list(x = x)
            pgfvalue(x, ...)
          })



#' @rdname pgfvalue
#' @examples
#' ## POSIXt values
#' pgfvalue(start = as.POSIXct('2016-04-01'), end = Sys.time())
#' 
#' @export
setMethod(f = 'pgfvalue',
          signature = 'POSIXt',
          definition = function(x, ...) {
            x <- format(x, '%Y-%m-%d')
            if ('x' %in% names(sys.call())) x <- list(x = x)
            pgfvalue(x, ...)
          })



#' @rdname pgfvalue
#' @export
setMethod(f = 'pgfvalue',
          signature = 'Date',
          definition = function(x, ...) {
            x <- format(x, '%Y-%m-%d')
            if ('x' %in% names(sys.call())) x <- list(x = x)
            pgfvalue(x, ...)
          })



#' @rdname pgfvalue
#' @export
setMethod(f = 'pgfvalue',
          signature = 'missing',
          definition = function(x, ...) {
            x <- list(...)
            pgfvalue(x)
          })



#' @rdname pgfvalue
#' @details Lists can be nested. An unnamed list element is expanded to its
#'  (named or unnamed) entries. For a named element, the value is pasted to a
#'  single character with its values separated by a comma. The values (if named)
#'  are prepended by the values name and an equal sign. If the value contains
#'  a comma or an equal sign, it is masked with curly brackets.
#' 
#' @examples
#' ## simple list as unnamed argument
#' pgfvalue(list(num_key = 12.3, log_key = TRUE, chr_key = 'text value'))
#' 
#' ## simple lists as named arguments
#' pgfvalue(list_1 = list(num = 2, val = 'text'),
#'  list_2 = list(date = Sys.Date(), description = 'current date'))
#' 
#' ## nested lists
#' pgfvalue(list_1 = list(num = 2, val = 'text',
#'  list_2 = list(date = Sys.Date(), description = 'current date')))
#' 
#' @export
setMethod(f = 'pgfvalue',
          signature = 'list',
          definition = function(x, ...) {
            if ('x' %in% names(sys.call())) x <- list(x = x)
            if (length(sys.call()) > 2) x <- c(x, list(...))
            
            x <- sapply(x, pgfvalue, simplify = FALSE, USE.NAMES = TRUE)
            x <- vapply(x, FUN = function(y) {
              if (!is.null(names(y))) {
                i_named <- (names(y) != '')
                i_mask <- grepl('[=,]', y) & (!grepl('^\\{.*?\\}$', y))
                y[i_named & i_mask] <- paste0('{', y[i_named & i_mask], '}')
                y[i_named] <- paste(names(y)[i_named], y[i_named], sep = ' = ')
              }
              y <- paste(y, collapse = ', ')
              return(structure(y, class = 'pgfvalue'))
            }, FUN.VALUE = character(1))
            return(structure(x, class = 'pgfvalue'))
          })




setOldClass('pgfvalue')
#' @rdname pgfvalue
#' @export
setMethod(f = 'pgfvalue',
          signature = 'pgfvalue',
          definition = function(x, ...) {
            x <- c(x, pgfvalue(...))
            return(structure(x, class = 'pgfvalue'))
          })



setOldClass('pgfkeyvalue')
#' @rdname pgfvalue
#' @export
setMethod(f = 'pgfvalue',
          signature = 'pgfkeyvalue',
          definition = function(x, ...) {
            return(structure(x, class = c('pgfvalue', 'pgfkeyvalue')))
          })




#' @rdname pgfvalue
#' @return The function \code{pgfkeyvalue()} returns an object of S3 class
#'  \code{pgfkeyvalue}. The structure of this class is the same as for class
#'  \code{pgfvalue} except for a different way of printing output (see
#'  \code{\link[=knit_print.pgfvalue]{knit_print}} for details).
#'  
#' @export
pgfkeyvalue <- function(...) {
  x <- structure(pgfvalue(...), class = c('pgfkeyvalue', 'pgfvalue'))
  return(x)
}
