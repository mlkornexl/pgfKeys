% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgfKeysSetValue.R
\docType{methods}
\name{pgfKeysSetValue}
\alias{pgfKeysSetValue}
\alias{pgfKeysSetValue,list-method}
\title{LaTeX-Command to Set pgf-Keys}
\usage{
pgfKeysSetValue(x, absolute = FALSE, ...)

\S4method{pgfKeysSetValue}{list}(x, absolute = FALSE, ...)
}
\arguments{
\item{x}{a list}

\item{absolute}{logical, see details}

\item{...}{additional parameter}
}
\description{
LaTeX-Command to Set pgf-Keys
}
\details{
The parameter \code{absolute} controls the handling of the key's
 path. If \code{absolute = TRUE} the key's path is set to an absolute path,
 i.e. a slash is prepended (if not already present). In any case, multiple
 leading slashes are converted to a single one.
 
 If \code{absolute = FALSE} absolute paths are converted to relative ones,
 i.e. any leading slashes are omitted.
 
 If \code{absolute} is missing, no changes regarding the path are made.
}
