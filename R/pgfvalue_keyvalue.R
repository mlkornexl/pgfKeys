#' Parsing Values for PGF Key-Value Pairs
#' 
#' The function parses values and key names to be used with PGF key-value
#' pairs by escaping LaTeX special characters, masking output with braces, or
#' checking valid input.
#' 
#' @param value value to be parsed, see methods for available classes
#' @param mask if \code{TRUE} and value contains \code{"="} or \code{","},
#'     \code{value} is masked by curly brackest \code{{...}}
#' @param ... additional parameter passed to S3 methods
#' 
#' @return an object of class \code{pgfvalue_value}; The initial class of
#'     \code{value} will be, if it does not already contain
#'     \code{pgfvalue_value} expanded by \code{pgfvalue_value}.
#' 
#' @rdname pgfvalue_value
#' @export
pgfvalue_value <- function(value, mask = FALSE, ...) {
  UseMethod("pgfvalue_value")
}



#' @rdname pgfvalue_value
#' 
#' @param na character, replacement value for \code{NA} characters
#' @param newlines boolean, controls escaping of \emph{single} new line characters,
#'     see details
#' @param spaces boolean, controls behaviour of multiple white spaces, see
#'     details
#' 
#' @details Special characters in character vectors \code{value} will be
#'     escaped:
#'     \enumerate{
#'         \item backslashes (\code{\\}) are replaced by \code{\\textbackslash}
#'         \item special characters (\code{#}, \code{$}, \code{\%}, \code{&},
#'             \code{_}, \code{\{}, \code{\}}) are escaped by prepending a
#'             backslash
#'         \item \code{\\textbackslash} is appended a pair of curly brackets
#'         \item tilde (\code{~}) and circumflex (\code{^}) are replaced by
#'             their LaTeX variants \code{\\textasciitilde} and
#'             \code{\\textasciicircum}
#'         \item carriage returns are replaced by a new line character
#'         \item any white spaces are replace by a single space
#'         \item any leading or trailing spaces will be removed
#'     }
#'     
#'     If \code{spaces = TRUE} any multiple spaces are escaped to
#'     \emph{protected} LaTeX spaces (or "control" spaces).
#'     
#'     If \code{newlines = TRUE} any \emph{single} line break will be replaces by
#'     a LaTeX linebreak (\code{\\}).
#' 
#' @method pgfvalue_value character
#' @export
pgfvalue_value.character <- function(value, mask = FALSE, na = NA_character_,
                                     newlines = FALSE, spaces = FALSE, ...) {
  
  old_class <- class(value)
  
  value <- value %>%  
    stringr::str_replace_all("\\\\", "\\\\textbackslash") %>%
    stringr::str_replace_all("([#$%&_{}])", "\\\\\\1") %>%
    stringr::str_replace_all("\\\\textbackslash", "\\\\textbackslash{}") %>%
    stringr::str_replace_all("~", "\\\\textasciitilde{}") %>%
    stringr::str_replace_all("\\^", "\\\\textasciicircum{}")  %>%
    stringr::str_replace_all("[\\r]+", "\n") %>%
    stringr::str_replace_all("\\s", " ") %>%
    stringr::str_trim()
  
  if (newlines) {
    value <- stringr::str_replace_all(value, "(?<!\\n])\\n(?!\\n)",
                                      "\\\\\\\\\n")
  }
  
  if (spaces) {
    value <- stringr::str_replace(value, "((?<=\\s)\\s|\\s(?=\\s))", "\\\\ ")
  }
  
  if ("pgfvalue_value" %in% old_class) {
    class(value) <- old_class
  } else {
    class(value) <- c("pgfvalue_character", 'pgfvalue_value', old_class)
  }
  
  value[is.na(value)] <- na
  
  if (mask) {
    return(pgfvalue_value(value))
  } else {
    return(value)
  }
}



#' @rdname pgfvalue_value
#' 
#' @return Return values that contain equal signs, colons, or new line
#'     characters can be masked with curly brackets if \code{mask = TRUE}.
#'     If \code{mask = TRUE} and return value \emph{is already} masked, not
#'     additional braces will be added. If \code{mask = TRUE} and return value
#'     \emph{does not} contain special characters, masking braces will be
#'     removed.
#'     
#'     For \code{value} of type \code{pgfvalue_value} and \code{mask = FALSE},
#'     the return value will remain unchanged.
#' 
#' @method pgfvalue_value pgfvalue_value
#' @export
pgfvalue_value.pgfvalue_value <- function(value, mask = FALSE, ...) {
  
  if (mask) {
    value <- value %>%
      stringr::str_replace("^\\s*[{](.*[=,\n].*)(?<!\\\\)[}]\\s*$", "\\1") %>%
      stringr::str_replace("^(.*[=,\n].*)$", "{\\1}") %>%
      stringr::str_replace("^[{]\\s*[{](.*?)[}]\\s*[}]", "{\\1}") %>%
      `class<-`(class(value))
  }
  
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @method  pgfvalue_value logical
#' @export
pgfvalue_value.logical <- function(value, mask = FALSE,
                                   na = NA_character_, ...) {
  value <- ifelse(value, "true", "false")
  value[is.na(value)] <- na
  class(value) <- c("pgfvalue_logical", "pgfvalue_value", class(value))
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @method  pgfvalue_value integer
#' @export
pgfvalue_value.integer <- function(value, mask = FALSE,
                                   na = NA_character_, ...) {
  value <- as.character(value)
  value[is.na(value)] <- na
  class(value) <- c("pgfvalue_integer", "pgfvalue_value", class(value))
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @return Numeric values will be returned in "fixed point" decimal notation
#'     with decimal mark being a dot.
#' 
#' @method  pgfvalue_value numeric
#' @export
pgfvalue_value.numeric <- function(value, mask = FALSE,
                                   na = NA_character_,  ...) {
  value <- format(value, scientific = FALSE, trim = TRUE, decimal.mark = ".")
  value[is.na(value)] <- na
  class(value) <- c("pgfvalue_numeric", "pgfvalue_value", class(value))
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @return Date values are returned in ISO 8601 format, see \code{\%F}
#'     formatting string  in \code{\link[base]{strptime}}.
#' 
#' @method pgfvalue_value Date
#' @export
pgfvalue_value.Date <- function(value, mask = FALSE,
                                na = NA_character_, ...) {
  value <- format(value, "%F")
  value[is.na(value)] <- na
  class(value) <- c("pgfvalue_Date", "pgfvalue_value", class(value))
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @param time boolean, controls return of time part
#' 
#' @return Date time values are returned as date values only if
#'     \code{time = FALSE} (the default). Otherwise, format will be
#'     \emph{\code{YYYY-MM-DDThh:mm:ss}}, see \code{\%F} and \code{\%X}
#'     formatting string in \code{\link[base]{strptime}}.
#'
#' @method pgfvalue_value POSIXt
#' @export
pgfvalue_value.POSIXt <- function(value, mask = FALSE, na = NA_character_,
                                  time = FALSE, ...) {
  if (time) {
    value <- format(value, "%FT%X")
  } else {
    value <- format(value, "%F")
  }
  value[is.na(value)] <- na
  class(value) <- c("pgfvalue_POSIXt", "pgfvalue_value", class(value))
  return(value)
}



#' @rdname pgfvalue_value
#' 
#' @param key character string, if of length greater one, only first entry
#'     will be used
#'     
#' @details Key names may contain any alpha-numeric characters as well as
#'     white spaces (leading and trailing ones will be omitted) and special
#'     characters \code{.}, \code{_}, \code{$}, or \code{/}. The latter two
#'     are used as path separators. Any part of \code{key} \emph{before} the
#'     last \code{/} or \code{$} is used as path.
#' 
#' @return Function \code{pgfvalue_key} returns a valid name of the key as an
#'     object of class \code{pgfvalue_key}. If a path name is included in
#'     \code{key}, see details, it is set as attribute \code{path}.
#' 
#' @export
pgfvalue_key <- function(key) {
  key <- as.character(key)[1]
  
  if (stringr::str_detect(key, "[^[:alnum:]\\s._$/]")) {
    stop("Invalid characters for pgfvalue-keys.")
  }
  
  path <- stringr::str_extract(key, ".*?(?=[$]+[^$]*$)") %>%
    stringr::str_replace_all("\\s", " ") %>%
    stringr::str_replace_all(" *[/$] *", "/") %>%
    stringr::str_trim()
  
  key <- stringr::str_extract(key, "[^$/]*$") %>%
    stringr::str_trim() %>%
    `class<-`(c("pgfvalue_key", class(key))) %>%
    `attr<-`(which = "path", value = path)

  return(key)
  
}




