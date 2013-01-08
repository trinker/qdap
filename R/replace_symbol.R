#' Replace Symbols With Word Equivalents
#' 
#' This function replaces symbols with word equivalents (e'g' \code{@@} becomes 
#' \code{"at"}.
#' 
#' @param text.var  The text variable.
#' @param dollar logical.  If TRUE replaces dollar sign ($) with \code{"dollar"}.
#' @param percent logical.  If TRUE replaces percent sign (%) with \code{"percent"}.
#' @param pound logical.  If TRUE replaces pound sign (#) with \code{"number"}.
#' @param atlogical.  If TRUE replaces at sign (@@) with \code{"at"}.
#' @param and logical.  If TRUE replaces and sign (&) with \code{"and"}.
#' @param with  logical.  If TRUE replaces with sign (w/) with \code{"with"}.
#' @return Returns a character vector with symbols replaced..
#' @keywords symbol-replace
#' @seealso 
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_number}},
#' \code{\link[qdap]{qprep}}
#' @export
#' @examples
#' \dontrun{
#' x <- c("I am @ Jon's & Jim's w/ Marry", "I owe $41 for food", "two is 10% 
#'     of a #")
#' replace_symbol(x)
#' }
replace_symbol <-
function(text.var, dollar = TRUE, percent = TRUE, 
         pound = TRUE, at = TRUE, and = TRUE, with = TRUE) {
  x <- c(dollar, percent, pound, at, and, with, with)
  scrubber(mgsub(pattern = c("%", "$", "#", "&", "@", "w/o", "w/")[x], 
        replacement = spaste(c("percent", "dollar", "number", "and", "at", 
        "without", "with")[x]), text.var = text.var, fixed = TRUE,
         leadspace = FALSE, trailspace = FALSE))
}

