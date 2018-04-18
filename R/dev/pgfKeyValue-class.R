#' Key-Value Pair for pgf-Keys
#' 
#' An S4 class to store a single key-value pair to use with pgf-keys.
#' 
#' @slot path character, path of the key
#' @slot key character, the name of the key
#' @slot handler character, key handler
#' @slot value character, the value of the key
#' @slot family character, optional, the family of the key(s)
#' 
#' @exportClass pgfKeyValue
#' @export pgfKeyValue
#' 
pgfKeyValue <- setClass(Class = 'pgfKeyValue',
                        slots = list(path = 'character',
                                     key = 'character',
                                     handler = 'character',
                                     value = 'character',
                                     family = 'character'),
                        prototype = list(family = ''))

setValidity(Class = 'pgfKeyValue',
            method = function(object) {
              TRUE
            })

setMethod(f = 'initialize',
          signature = 'pgfKeyValue',
          definition = function(.Object, key, path, handler, value,
                                expand = TRUE, ...) {
            if (!missing(key)) pgfKey(.Object, expand = expand) <- key
            if (!missing(value)) pgfValue(.Object) <- value
            if (!missing(path)) pgfPath(.Object) <- path
            if (!missing(handler)) pgfHandler(.Object) <- handler
            
            return(.Object)
          })

setIs(class1 = 'pgfKeyValue',
      class2 = 'character',
      coerce = function(from) {
        to <- pgfKey(from, na_value = NA_character_, include_handler = TRUE,
                     include_path = TRUE)
        from@value <- pgfValue(from, na_value = NA_character_)
        
        i <- (!is.na(to)) & (!is.na(from@value))
        to[i] <- paste(to[i], from@value[i], sep='=')
        
        return(to)
      },
      replace = function(from, value) {
        pgfKey(from, expand = TRUE) <- value
        return(from)
      })
