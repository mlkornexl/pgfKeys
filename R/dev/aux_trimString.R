#' Trim String
#' 
#' Auxilliary function to remove white spaces in a string.
#' 
#' @param x any vector that can be coerced to a string, names are preserved
#' @param rm_empty logical, if \code{TRUE}, empty strings are removed; If
#'  \code{x} constists only of empty strings and \code{rm_empty = TRUE}, a 
#'  character-string of length 0 is returned.
#' @param na_value character, missing values (\code{NA}) are replaced by
#'  \code{na_value}; If \code{na_value = NULL}, \code{NA} values are omitted.
#' @param trim.all logical, short hand for setting \code{trim.lead},
#'  \code{trim.mid}, and \code{trim.trail}
#' @param trim.margin logical, short hand for setting \code{trim.lead},
#'  and \code{trim.trail}
#' @param trim.lead logical, if \code{TRUE} all \emph{leading} white spaces
#'  are omitted
#' @param trim.mid logical, if \code{TRUE} all (mulitple) white spaces
#'  are replaced by a single space
#' @param trim.trail logical, if \code{TRUE} all \emph{trailing} white spaces
#'  are omitted
#' 
#' @return The function returns a character vector. Missing values are processed
#'  according to the parameter \code{na_value}. If \code{rm_empty = TRUE}, empty
#'  strings are omitted, i.e. the result could be a charater vector of length 0.
#'  If \code{rm_empty = FALSE} the returned vector is at least of length 1 (even
#'  if the input was of length 0 or contained only missing values).
#'  
#'  If \code{x} was a named vector (or could be coerced to one), the names are
#'  preserved.
#' 
#' @author M. Kornexl
#' 
#' @rdname aux_trimString
#' @export
#' 
.trimString <- function(x, rm_empty = FALSE, na_value = '',
                        trim.all = FALSE, trim.margin = TRUE,
                        trim.lead = (trim.all | trim.margin),
                        trim.mid = trim.all,
                        trim.trail = (trim.all | trim.margin)) {
  x <- setNames(as.character(x), names(x))
  
  if (is.null(na_value)) x <- x[!is.na(x)] else x[is.na(x)] <- na_value
  
  if (trim.lead) x <- sub('^[[:space:]]+', '', x)
  if (trim.trail) x <- sub('[[:space:]]+$', '', x)
  if (trim.mid) x <- gsub('[[:space:]]+', ' ', x)
  
  if (rm_empty) {
    x <- x[x != '']
  } else if (length(x) == 0) {
    x <- ''
  }
  
  return(x)
}
