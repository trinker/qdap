#' Quick Preparation of Text
#'
#' Wrapper for bracketX, replace_number, replace_symbol, 
#' replace_abbreviation and scrubber to quickly prepares text for 
#' analysis.  Care should taken with this function to ensure data 
#' is properly formatted and complete.
#' 
#' @param \ldots name(s) of package(s)
#' @param install logical.  If TRUE will attempt to install a package 
#' not found in the library
#' @param update logical.  If TRUE will attempt to update out of date packages
#' @param require logical.  If TRUE will use require; FALSE will use library
#' @keywords library require package update
#' @seealso 
#' \code{\link[base]{library}},
#' \code{\link[base]{require}},
#' \code{\link[utils]{install.packages}}
#' @examples
#' x <- "I like 60 (laughter) #d-bot and $6 @ the store w/o 8p.m."
#' qprep(x)
qprep <-
function(text.var, rm.dash = TRUE, bracket = "all", missing = NULL, 
    names = FALSE, abbreviation = qdap::abbreviations, 
    replace = NULL, ignore.case = TRUE, num.paste = "separate") {
    x <- bracketX(clean(text.var), bracket = bracket, 
        missing = missing, names = names)
    x <- replace_abbreviation(x, abbreviation = abbreviation, 
        replace = replace, ignore.case = ignore.case)
    x <- replace_number(x, num.paste = num.paste)
    if (rm.dash) {
        x <- gsub("-", " ", x)
    }
    Trim(scrubber(replace_symbol(x)))
}
