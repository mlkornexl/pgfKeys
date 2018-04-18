#' Level of Masking
#' 
#' The function returns the level of masking within a string. A string is said
#' to be masked, if it is enclosed by opening and closing character
#' (parantheses). A text that is enclosed by a single pair of parantheses has
#' masking level of one. A mask text inside another masking has level two, and
#' so on.
#' 
#' @param x character vector containing the string that is inspected
#' @param p_open single character, opening delimiter of masked text (left
#'  paranthesis)
#' @param p_close single character, closing delimiter of masked text (right
#'  paranthesis)
#' @param simplify logical, if \code{TRUE} and \code{x} is of length one, a
#'  single numeric vector is returned instead of a list.
#' 
#' @details The function also tests for unmatched parantheses, i.e. an opening
#'  paranthesis not followed by a closing one or a closing paranthesis without
#'  a preceding opening on.
#'  
#'  If \code{p_close = p_open} (e.g. verbatim-masking or in-line equations in
#'  LaTeX) the delimiters are treated as opening an closing alternating.
#'  Therefore the maximum masking level is 1 in this case.
#' 
#' @return The function returns a list of length equal to the length of the
#'  character vector \code{x}. Each element contains an integer vector of the
#'  same length as the corresponding element of \code{x}. 
#'  
#'  If \code{simplify = TRUE} and length of \code{x} is one, an integer vector
#'  is returned.
#'  
#'  In the case of unmatched parantheses, an error is thrown.
#' 
#' @rdname aux_levelParantheses
#' @export
#' 
.levelParantheses <- function(x, p_open, p_close = p_open, simplify = FALSE) {
  
  i_open <- gregexpr(p_open, x, fixed = TRUE)
  n <- lapply(x, nchar)
  if (p_open == p_close) {
    i_level <- mapply(function(n, i_open) {
      
      i_open <- i_open[i_open > 0]
      
      if (length(i_open) %% 2 == 1) stop('Unmatched parantheses!\n',
                                         'Uneven number of parantheses.')
      i_level <- rep(0, n)
      if (length(i_open) > 0) {
        i_level[i_open] <- rep(c(1, -1), length.out = length(i_open))
        i_level <- cumsum(i_level)
        i_level[i_open] <- 1
      }
      
      return(i_level)
    }, n = n, i_open = i_open, SIMPLIFY = FALSE)
  } else {
    i_close <- gregexpr(p_close, x, fixed = TRUE)
    
    i_level <- Map(function(n, i_open, i_close) {
      if (n == 0) return(vector('numeric'))
      
      i_open <- i_open[i_open > 0]
      i_close <- i_close[i_close > 0]
      
      i_level <- rep(0, n)
      i_level[i_open] <- 1
      i_level[i_close] <- -1
      
      i_level <- cumsum(i_level)
      
      if (any(i_level < 0)) stop('Unmatched parantheses!\n',
                                 'Missing opening paranthesis.')
      if (i_level[n] > 0) stop('Unmatched parantheses!',
                               'Missing closing paranthesis.')
      
      i_level[i_close] <- i_level[i_close] + 1
      
      return(i_level)
    }, n = n, i_open = i_open, i_close = i_close)
  }
  
  if (simplify & length(i_level) == 1) i_level <- i_level[[1]]
  return(i_level)
}

#' Mask Text
#' 
#' The function masks text enclosed by opening and closing delimiters
#' (parantheses).
#' 
#' @inheritParams .levelParantheses
#' @param level integer, text with masking level equal to or higher than
#'  \code{level} will be masked
#' @param mask_char character, masked text will be replaced by this character.
#' 
#' @details Opening and closing masking identifier as well as the masking
#'  character \code{mask_char} need to be single characters, such that the
#'  text after masking is of the same length as the original text.
#' 
#' @return The function returns a character vector of length equal to the
#'  length of \code{x}. Each element contains a string with masked text of level
#'  \code{level} or higher replaced by \code{mask_char}. Opening and closing
#'  masking identifier are masked as well. Therefore the returned string will
#'  be of same length as the original string.
#'  
#'  In the case of unmatched parantheses, an error is thrown.
#' 
#' @seealso See \code{\link{.levelParantheses}} for the calculation of masking
#'  levels.
#' 
#' @rdname aux_maskParantheses
#' @export
#' 
.maskParantheses <- function(x, p_open, p_close, level = 1, mask_char = '#') {
  
  x <- Map(function(x, i, level, mask_char) {
    x[i >= level] <- mask_char
    x <- paste(x, collapse = '')
    return(x)
  },
  x = strsplit(x, split = ''),
  i = .levelParantheses(x, p_open, p_close, simplify = FALSE),
  level = level,
  mask_char = mask_char)
  
  return(unlist(x))
}
