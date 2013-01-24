#' Strip Text 
#' 
#' Strip text of unwanted characters.
#' 
#' @param x The text variable.
#' @param char.keep A character vector of symbols (i.e. punctuation) that 
#' \code{strip} should keep.  The default is to strip every symbol except 
#' apostrophes and a double tilde \code{"~~"}.  The double tilde \code{"~~"} is 
#' included for a convenient means of keeping word groups together in functions
#' that split text apart based on spaces.
#' @param digit.remove logical.  If TRUE strips digits from the text.
#' @param apostrophe.remove logical.  If TRUE removes apostrophes from the 
#' output.
#' @param lower.case logical.  If TRUE forces all alpha characters to lower case.
#' @return Returns a vector of text that has been stripped of unwanted characters.
#' @seealso \code{\link[qdap]{stopwords}}
#' @export
#' @examples
#' \dontrun{
#' strip(DATA$state)
#' strip(DATA$state, apostrophe.remove=FALSE)
#' strip(DATA$state, char.keep = c("?", "."))
#' }
strip <-
function (x, char.keep = "~~", digit.remove = TRUE, apostrophe.remove = TRUE,
    lower.case = TRUE) {
    strp <- function(x, digit.remove, apostrophe.remove, char.keep, lower.case) {
        if (!is.null(char.keep)) {
            x2 <- Trim(gsub(paste0(".*?($|'|",
            paste(paste0("\\", char.keep), collapse = "|"),
            "|[^[:punct:]]).*?"), "\\1", 
                as.character(x)))
        } else {
            x2 <- Trim(gsub(".*?($|'|[^[:punct:]]).*?", "\\1", 
                as.character(x)))
        }
        if (lower.case) {
            x2 <- tolower(x2)
        }
        if (apostrophe.remove) {
            x2 <- gsub("'", "", x2)
        }
        ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", x2), x2)
    }
    x <- clean(gsub("/", " ", gsub("-", " ", x))) 
    unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
       apostrophe.remove = apostrophe.remove, char.keep = char.keep, 
       lower.case = lower.case))))
}
