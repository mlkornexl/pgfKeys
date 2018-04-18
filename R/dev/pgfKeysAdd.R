#' @include pgfKeyValue-class.R
NULL

#' Add pgf-Keys
#' 
#' The function add pgf-keys to an \code{pgfKeyValue}-object.
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param ... additional parameter, not yet used
#' @param value keys to be added
#' 
#' @docType methods
#' @exportMethod pgfKeysAdd<-
#' 
setGeneric(name = 'pgfKeysAdd<-',
           def = function(x, ..., value) standardGeneric('pgfKeysAdd<-'))

#' @rdname pgfKeysAdd-set
setReplaceMethod(f = 'pgfKeysAdd',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'pgfKeyValue'),
                 definition = function(x, ..., value) {
                   x <- .pgfSyncSlots(x)
                   value <- .pgfSyncSlots(value)
                   
                   if ((x@family != value@family) & (value@family != '')) {
                     x <- .pgfExpandFamily(x)
                     value <- .pgfExpandFamily(value)
                   }
                   
                   x@path <- c(x@path, value@path)
                   x@key <- c(x@key, value@key)
                   x@handler <- c(x@handler, value@handler)
                   x@value <- c(x@value, value@value)
                   
                   return(x)
                 })

#' @rdname pgfKeysAdd-set
#' @description For \code{value} a \code{character}-vector, it is converted to
#'  a \code{pgfKeyValue}-object using the \code{\link{pgfKeyValueSet<-}}
#'  function.
setReplaceMethod(f = 'pgfKeysAdd',
                 signature = signature(x = 'pgfKeyValue',
                                       value = 'character'),
                 definition = function(x, ..., value) {
                   value <- pgfKeyValue(key = value, expand = TRUE)
                   pgfKeysAdd(x, ...) <- value
                   return(x)
                 })

#' Expand Family
#' 
#' The function prepends the family of a \code{pgfKeyValue}-object to each of
#' key's paths. The family-slot then is set to an empty string.
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param ... additional parameter, not yet used
#' 
#' @return The function returns an object of type \code{pgfKeyValue}.
#' 
#' @rdname aux_pgfExpandFamily
#' 
.pgfExpandFamily <- function(x, ...) {
  if (x@family == '') return(x)
  
  i_abs <- grepl('^[[:space:]]/', x@key)
  x@path[i_abs] <- paste(x@family, x@path[i_abs], sep = '/')
  x@family <- ''
  
  return(x)
}

#' Synchronize Slots
#' 
#' The function synchronizes the length of the individual slots. The length
#' of the slots \code{path}, \code{handler}, and \code{value} are aligned with
#' the length of the slot \code{key}.
#' 
#' @param x an object of type \code{pgfKeyValue}
#' @param ... additional parameter, not yet used
#' 
#' @return The function returns an object of type \code{pgfKeyValue}.
#' 
#' @rdname aug_pgfSyncSlots
#' 
.pgfSyncSlots <- function(x, ...) {
  n <- length(x@key)
  
  x@family <- x@family[1]
  x@path <- rep(x@path, length.out = n)
  x@handler <- rep(x@handler, length.out = n)
  x@value <- rep(x@value, length.out = n)
  
  return(x)
}