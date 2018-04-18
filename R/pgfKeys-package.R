#' pgfKeys: A package for Setting pgf-Keys in LaTeX
#' 
#' The package provide functions to format and output variables for the use
#' with pgf-keys in LaTeX
#' 
#' @docType package
#' @name pgfKeys-package
NULL

#' @importFrom methods setGeneric setMethod

.onLoad <- function(libname, pkgname) {
#   # DELETE: readRenviron(file.path('inst', 'config', 'Environ.pgfkeys'))
#   readRenviron(system.file(file.path('config', 'Environ.pgfkeys'),
#                            package=pkgname))
}

.onUnload <- function(libpath) {
#   i <- names(Sys.getenv())
#   i <- i[grep('^PGFKEYS_', i)]
#   Sys.unsetenv(i)
}
