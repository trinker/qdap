#' Quick Preparation of Text
#'
#' Wrapper for \code{bracketX}, \code{replace_number}, \code{replace_symbol}, 
#' \code{replace_abbreviation} and \code{scrubber} to quickly prepare text for 
#' analysis.  Care should taken with this function to ensure data is properly 
#' formatted and complete.
#' 
#' @param text.var The text variable.    
#' @param rm.dash logical.  If TRUE dashes will be removed.
#' @param bracket The type of bracket (and encased text) to remove.  This is one 
#' of the strings \code{"curly"}, \code{"square"}, \code{"round"}, 
#' \code{"angle"} and \code{"all"}.  These strings correspond to: \{, [, (, < 
#' or all four types.  Also takes the argument \code{NULL} which turns off this 
#' parsing technique.
#' @param missing Value to assign to empty cells.
#' @param names logical.  If TRUE the sentences are given as the names of the 
#' counts.
#' @param abbreviation A two column key of abbreviations (column 1) and long 
#' form replacements (column 2) or a vector of abbreviations.  Default is to use 
#' qdap's abbreviations data set.  Also takes the argument \code{NULL} which 
#' turns off this parsing technique.
#' @param replace A vector of long form replacements if a data frame is not 
#' supplied to the abbreviation argument.
#' @param ignore.case logical.  If TRUE replaces without regard to 
#' capitalization.
#' @param num.paste A character string c(\code{"separate"}, \code{"combine"}); 
#' \code{"separate"} will treat each word section as separate, \code{"combine"} 
#' will lump the sections together as one word.  Also takes the argument 
#' \code{NULL} which turns off this parsing technique.
#' @seealso 
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_number}},
#' \code{\link[qdap]{replace_symbol}}
#' @note Care should taken with this function to ensure data is properly 
#' formatted and complete.
#' @export
#' @examples
#' \dontrun{
#' x <- "I like 60 (laughter) #d-bot and $6 @@ the store w/o 8p.m."
#' qprep(x)
#' }
qprep <-
function(text.var, rm.dash = TRUE, bracket = "all", missing = NULL, 
    names = FALSE, abbreviation = qdap::abbreviations, 
    replace = NULL, ignore.case = TRUE, num.paste = "separate") {
  
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
    Trim(scrubber(replace_symbol(text.var)))
}