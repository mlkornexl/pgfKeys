#' @include pgfKeyValue-class.R
NULL

#' Set Key-Value Pair
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param key optional a character specifying the key (which might itself
#'  include path and handler)
#' @param value character containing key-value pairs, see details for the
#'  format of theses strings
#' @param ... additional parameter, not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfKeyValue<-
#' 
setGeneric(name = 'pgfKeyValue<-',
           def = function(x, key, ..., value) {
             standardGeneric('pgfKeyValue<-')
           })

#' @rdname pgfKeyValue-set
setReplaceMethod(f = 'pgfKeyValue',
                 signature = signature(x = 'pgfKeyValue',
                                       key = 'character',
                                       value = 'character'),
                 definition = function(x, key, ..., value) {
                   pgfKey(x) <- key
                   x@value <- .trimString(value, na_value = NA_character_)
                   return(x)
                 })

#' @rdname pgfKeyValue-set
setReplaceMethod(f = 'pgfKeyValue',
                 signature = signature(x = 'pgfKeyValue',
                                       key = 'missing',
                                       value = 'character'),
                 definition = function(x, key, ..., value) {
                   
                   pgfKey(x) <- value
                   pgfValue(x) <- pgfValue(value, na_value = NA_character_)

                   return(x)
                 })

#' @rdname pgfKeyValue-set
setReplaceMethod(f = 'pgfKeyValue',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'list'),
                 definition = function(x, key, ..., value) {
                   return(x)
                 })

#' Set Value of pgf-Keys
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param value character, value of the pgf-key
#' @param ... additional parameter not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfValue<-
#' 
setGeneric(name = 'pgfValue<-',
           def = function(x, ..., value) standardGeneric('pgfValue<-'))

#' @rdname pgfValue-set
setReplaceMethod(f = 'pgfValue',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'character'),
                 definition = function(x, ..., value) {
                   value <- .trimString(value, na_value = NA_character_)
                   value <- sub('^\\{(.*)\\}$', '\\1', value)
                   x@value <- value
                   return(x)
                 })

#' @rdname pgfValue-set
setReplaceMethod(f = 'pgfValue',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'list'),
                 definition = function(x, ..., value) {
                   kv <- vapply(value, function(x_value) {
                     kv <- pgfKeyValue()
                     pgfKeys(kv, expand = TRUE) <- x_value
                     kv <- paste(as(kv, 'character'), sep = ', ')
                     kv_masked <- gsub('\\\\(\\{|\\})', '--', kv)
                     kv_masked <- .maskParantheses(kv_masked, '{', '}')
                     if (grepl('[=,]', kv_masked)) kv <- paste0('{', kv, '}')
                     return(kv)
                   }, FUN.VALUE = character(1), USE.NAMES = FALSE)
                   
                   x@value <- kv
                 })

#' Set pgf-Keys
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param value character, name of the pgf-key (might include path and handler
#'  as well)
#' @param expand logical, if \code{TRUE} the value is expanded and path,
#'  handler, and value are stored as well
#' @param ... additional parameter not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfKey<-
#' 
setGeneric(name = 'pgfKey<-',
           def = function(x, expand = TRUE, ..., value) {
             standardGeneric('pgfKey<-')
           })

#' @rdname pgfKey-set
setReplaceMethod(f = 'pgfKey',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'ANY'),
                 definition = function(x, expand, ..., value) {
                   value <- as.character(value)
                   
                   x@key <- pgfKey(value, na_value = NA_character_,
                                   include_path = FALSE,
                                   include_handler = FALSE)
                   
                   if (expand) {
                     x@path <- sub('/+$', '',
                                   pgfPath(value, na_value = NA_character_))
                     x@handler <- sub('^/\\.', '',
                                      pgfHandler(value,
                                                 na_value = NA_character_))
                     x@value <- pgfValue(value, na_value = NA_character_)
                   }
                   
                   return(x)
                 })

#' @rdname pgfKey-set
setReplaceMethod(f = 'pgfKey',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'list'),
                 definition = function(x, expand, ..., value) {
                   pgfKey(x, expand = TRUE) <- pgfKey(value, ...)
                   
                   value <- vapply(value, function(x_value) {
                     kv <- pgfKeyValue()
                     pgfKey(kv, expand = TRUE) <- x_value
                     kv <- as(kv, 'character')
                     kv <- kv[!is.na(kv)]
                     if (length(kv) > 0) {
                       kv <- paste(as(kv, 'character'), sep = ', ')
                       kv_masked <- gsub('\\\\(\\{|\\})', '--', kv)
                       kv_masked <- .maskParantheses(kv_masked, '{', '}')
                       if (grepl('[=,]', kv_masked)) kv <- paste0('{', kv, '}')
                     } else {
                       kv <- NA_character_
                     }
                     return(kv)
                   }, FUN.VALUE = character(1), USE.NAMES = FALSE)
                   
                   x@value <- value
                   
                   return(x)
                 })

#' Set Handler for pgf-Keys
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param value character, handler for the pgf-key
#' @param ... additional parameter not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfHandler<-
#' 
setGeneric(name = 'pgfHandler<-',
           def = function(x, ..., value) standardGeneric('pgfHandler<-'))

#' @rdname pgfHandler-set
setReplaceMethod(f = 'pgfHandler',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'character'),
                 definition = function(x, ..., value) {
                   value <- .trimString(value, na_value = NA_character_)
                   value <- sub('^/?\\.', '', value)
                   
                   x@handler <- unname(value)
                   
                   return(x)
                 })

#' Set Path of pgf-Keys
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param value character, path of the pgf-key
#' @param ... additional parameter not yet implemented
#' 
#' @docType methods
#' @exportMethod pgfPath<-
#' 
setGeneric(name = 'pgfPath<-',
           def = function(x, ..., value) standardGeneric('pgfPath<-'))

#' @rdname pgfPath-set
setReplaceMethod(f = 'pgfPath',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'character'),
                 definition = function(x, ..., value) {
                   value <- .trimString(value, na_value = NA_character_)
                   value <- sub('/+$', '', value)
                   
                   x@path <- value
                   
                   return(x)
                 })
