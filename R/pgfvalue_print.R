#' Print PGF Key-Value Pairs
#' 
#' This printing function can be used for dynamic LaTeX documents to set
#' PGF keys using \command{pgfkeyssetvalue}.
#' 
#' @param x object
#' @param options knitr options
#' @param ... additional arguments
#' 
#' @rdname pgfvalue_print
#' @export
pgfvalue_print <- function(x, options, ...) UseMethod("pgfvalue_print", x)

#' @method pgfvalue_print default
#' @export
pgfvalue_print.default <- function(x, options, ...) {
  
  key <- deparse(substitute(x))

  if (!length(intersect(class(x),
                        stringr::str_extract(methods(pgfvalue_value),
                                             "(?<=^pgfvalue_value\\.).*")))) {
    stop("'pgfvalue_print.default' can only be applied to object classes",
         "for which a an explicit 'pgfvalue_value' method is defined.")
  }
  
  if (!is.null(options$na.rm) && options$na.rm) x <- x[!is.na(x)]
  
  if (!length(x)) return(knitr::asis_output(""))

  if (is.null(options$path)) {
    path <- ""
  } else {
    path <- options$path %>%
      stringr::str_remove("/+$") %>%
      stringr::str_c("/")
  }
  
  key <- pgfvalue_key(key)
    
  if (!is.null(options$aut_path)
      && options$auto_path
      && !is.na(attr(key, "path"))) {
    path <- stringr::str_c(path, attr(key, "path"))
  }
  
  x <- stringr::str_c(x, collapse = ", ")
  
  sprintf("\\pgfkeyssetvalue{%s%s}{%s}\n",
          path, key, pgfvalue_value(x, mask = FALSE)) %>%
    knitr::asis_output()

}



#' @rdname pgfvalue_print
#' @method pgfvalue_print list
#' @export
pgfvalue_print.list <- function(x, options, ...) {
  if (is.null(names(x)) || any(names(x) == "")) {
    stop("Only named lists may be used for 'pgfvalue' printing method.")
  }
  
  if (is.null(options$path)) {
    path <- ""
  } else {
    path <- options$path %>%
      stringr::str_remove("/+$") %>%
      stringr::str_c("/")
  }
  
  sprintf("\\pgfkeyssetvalue{%s%s}{%s}\n", path,
          purrr::map_chr(names(x), pgfvalue_key),
          purrr::map_chr(x, pgfvalue_value)) %>%
    stringr::str_c(collapse = "") %>%
    knitr::asis_output()
}



#' @rdname pgfvalue_print
#' @method pgfvalue_print InsuredOrPrincipal
#' @export
pgfvalue_print.InsuredOrPrincipal <- function(x, options, ...) {
  value <- list(
    GenderCd = x$InsuredOrPrincipalInfo$PersonInfo$GenderCd$Value,
    BirthDt = x$InsuredOrPrincipalInfo$PersonInfo$BirthDt$Value,
    
    TitlePrefix = x$GeneralPartyInfo$NameInfo$PersonName$TitlePrefix$Value,
    NameSuffix = x$GeneralPartyInfo$NameInfo$PersonName$NameSuffix$Value,
    GivenName = x$GeneralPartyInfo$NameInfo$PersonName$GivenName$Value,
    SurName = x$GeneralPartyInfo$NameInfo$PersonName$Surname$Value,
    
    StreetName = x$GeneralPartyInfo$Addr$DetailAddr[[1]]$StreetName$Value,
    StreetNumber = x$GeneralPartyInfo$Addr$DetailAddr[[1]]$StreetNumber$Value,
    UnitNumber = x$GeneralPartyInfo$Addr$DetailAddr[[1]]$UnitNumber$Value,
    PostalCode = x$GeneralPartyInfo$Addr$PostalCode$Value,
    City = x$GeneralPartyInfo$Addr$City$Value,
    CountryCd = x$GeneralPartyInfo$Addr$CountryCd$Value,
    
    EmailAddr = x$GeneralPartyInfo$Communications$EmailInfo$EmailAddr$Value
  )
  
  if (is.null(value$GenderCd)) value$GenderCd <- 0
  if (is.null(value$BirthDt)) value$BirthDt <- as.POSIXct(NA)
  
  value$PhoneNumber <- with(
    x$GeneralPartyInfo$Communications,
    {
      ii <- stringr::str_which(PhoneInfo$PhoneTypeCd$Value,
                               fixed("phone", ignore_case = TRUE))[1]
      PhoneInfo[ii, ]$PhoneNumber$Value
    }
  )
      
  value$FaxNumber <- with(
    x$GeneralPartyInfo$Communications,
    {
      ii <- stringr::str_which(PhoneInfo$PhoneTypeCd$Value,
                               fixed("fax", ignore_case = TRUE))[1]
      PhoneInfo[ii, ]$PhoneNumber$Value
    }
  )
  
  value$FullName <- c(value$GivenName, value$SurName) %>%
    .[!is.na(.)] %>%
    stringr::str_c(collapse = " ") %>%
    stringr::str_remove("(^\\s+|(?<=\\s)\\s+|\\s+$)")
  
  if (!length(value$FullName)) value$FullName <- NA_character_
  
  value$FullNameTitle <- value$FullName
  
  if (!is.na(value$TitlePrefix))
    value$FullNameTitle <- stringr::str_c(value$TitlePrefix,
                                          value$FullNameTitle, sep = " ")
  
  if (!is.na(value$NameSuffix))
    value$FullNameTitle <- stringr::str_c(value$FullNameTitle,
                                          value$NameSuffix, sep = ", ")
  
  value$FullNameTitle <- value$FullNameTitle %>%
    stringr::str_remove("(^\\s+|(?<=\\s)\\s+|\\s+$)")
  
  value$AddressLine <- value$StreetName
  if (!(is.na(value$StreetNumber) && is.na(value$UnitNumber))) {
    value$AddressLine <- stringr::str_c(
      value$AddressLine,
      c(value$StreetNumber, value$UnitNumber) %>%
        .[!is.na(.)] %>%
        stringr::str_c(collapse = "/"),
      sep = " "
    )
  }
  value$AddressLine <- value$AddressLine %>%
    stringr::str_remove("(^\\s+|(?<=\\s)\\s+|\\s+$)")
  
  pgfvalue_print(value, options = options)
}
