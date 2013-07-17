#' Replace Spaces
#' 
#' Replace spaces in words groups that should be grouped together.
#' 
#' @param text.var The text variable.    
#' @param terms A character vector of grouped word terms to insert a new 
#' separating/space character.
#' @param sep A character string to separate the terms.
#' @param rm.extra logical.  Should trailing, leading and > 1 continuous white 
#' spaces be removed?
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case sensitive 
#' and if \code{TRUE}, case is ignored during matching.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as 
#' is. Overrides all conflicting arguments.
#' @param \ldots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a character vector with extra, trailing and/or leading spaces
#' removed.
#' @note \code{link[qdap]{strip}} by default does not remove the double tilde 
#' \code{"~~"} character.
#' @details \code{\link[qdap]{space_fill}} is useful for keeping grouped words 
#' together.  Many functions in qdap take a \code{char.keep} or 
#' \code{char2space} argument.  This can be used to prepare multi word phrases 
#' (e.g., proper nouns) as a single unit.
#' @export
#' @examples
#' \dontrun{
#' x <- c("I want to hear the Dr. Martin Luther King Jr. speech.",
#'     "I also want to go to the white House to see President Obama speak.")
#' 
#' keeps <- c("Dr. Martin Luther King Jr.", "The White House", "President Obama")
#' space_fill(x, keeps)
#' strip(space_fill(x, keeps))
#' }
space_fill <- function(text.var, terms, sep = "~~", rm.extra = TRUE, 
    ignore.case = TRUE, fixed = FALSE, ...) {
    if (rm.extra) {
        terms <- Trim(reducer(terms))
    }
    reps <- gsub("\\s", sep, terms, ignore.case = ignore.case, ...)
    mgsub(terms, reps, text.var, ignore.case = ignore.case, fixed = fixed, ...)
}

