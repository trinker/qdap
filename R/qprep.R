#' Quick Preparation of Text
#'
#' Wrapper for \code{\link[qdap]{bracketX}}, \code{\link[qdap]{replace_number}}, 
#' \code{\link[qdap]{replace_symbol}}, \code{\link[qdap]{replace_abbreviation}} 
#' and \code{\link[qdap]{scrubber}} to quickly prepare text for analysis.  Care 
#' should be taken with this function to ensure data is properly formatted and 
#' complete.
#' 
#' @param text.var The text variable.    
#' @param rm.dash logical.  If \code{TRUE} dashes will be removed.
#' @param bracket The type of bracket (and encased text) to remove.  This is one 
#' of the strings \code{"curly"}, \code{"square"}, \code{"round"}, 
#' \code{"angle"} and \code{"all"}.  These strings correspond to: \{, [, (, < 
#' or all four types.  Also takes the argument \code{NULL} which turns off this 
#' parsing technique.
#' @param missing Value to assign to empty cells.
#' @param names logical.  If \code{TRUE} the sentences are given as the names of 
#' the counts.
#' @param abbreviation A two column key of abbreviations (column 1) and long 
#' form replacements (column 2) or a vector of abbreviations.  Default is to use 
#' qdap's abbreviations data set.  Also takes the argument \code{NULL} which 
#' turns off this parsing technique.
#' @param replace A vector of long form replacements if a data frame is not 
#' supplied to the abbreviation argument.
#' @param ignore.case logical.  If \code{TRUE} replaces without regard to 
#' capitalization.
#' @param num.paste logical.  If \code{TURE} a the elements of larger numbers are 
#' separated with spaces.  If \code{FALSE} the elements will be joined without 
#' spaces.  Also takes the argument \code{NULL} which turns off this parsing 
#' technique.
#' @param \ldots Other arguments passed to \code{\link[qdap]{replace_symbol}}.
#' @seealso 
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_number}},
#' \code{\link[qdap]{replace_symbol}}
#' @note Care should be taken with this function to ensure data is properly 
#' formatted and complete.
#' @export
#' @examples
#' \dontrun{
#' x <- "I like 60 (laughter) #d-bot and $6 @@ the store w/o 8p.m."
#' qprep(x)
#' }
qprep <-
function(text.var, rm.dash = TRUE, bracket = "all", missing = NULL, 
    names = FALSE, abbreviation = qdapDictionaries::abbreviations, 
    replace = NULL, ignore.case = TRUE, num.paste = TRUE, ...) {
  
    if (!is.null(bracket)) {
        text.var <- bracketX(clean(text.var), bracket = bracket, 
            missing = missing, names = names)
    }
    if (!is.null(abbreviation)) {
        text.var <- replace_abbreviation(text.var, abbreviation = abbreviation, 
            replace = replace, ignore.case = ignore.case)
    }
    if (!is.null(num.paste)) {
        text.var <- replace_number(text.var, num.paste = num.paste)
    }
    if (rm.dash) {
        text.var <- gsub("-", " ", text.var)
    }
    Trim(scrubber(replace_symbol(text.var, ...)))
}
