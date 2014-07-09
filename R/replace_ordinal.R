#' Replace Mixed Ordinal Numbers With Text Representation
#' 
#' Replaces mixed text/numeric represented ordinal numbers with words (e.g., 
#' "1st" becomes "first").
#' 
#' @param text.var  The text variable.
#' @param num.paste logical.  If \code{TRUE} a the elements of larger numbers are 
#' separated with spaces.  If \code{FALSE} the elements will be joined without 
#' spaces.
#' @param remove logical.  If \code{TRUE} ordinal numbers are removed from the text.
#' @keywords ordinal-to-word
#' @note Currently only implemented for ordinam values: 1-25, 30, 40, 50, 60, 70, 80, 90, & 100
#' @seealso 
#' \code{\link[qdap]{bracketX}},
#' \code{\link[qdap]{qprep}},
#' \code{\link[qdap]{replace_abbreviation}},
#' \code{\link[qdap]{replace_contraction}},
#' \code{\link[qdap]{replace_symbol}},
#' \code{\link[qdap]{replace_number}}
#' \code{\link[english]{english}}
#' @export
#' @examples
#' \dontrun{
#' x <- "I like the 1st one not the 22nd one."
#' replace_ordinal(x)
#' }
replace_ordinal <- function(text.var, num.paste = TRUE, remove = FALSE) {

    symb <- c("1st", "2nd", "3rd", paste0(4:20, "th"), "21st", "22nd", "23rd", 
        "24th", "25th", paste0(seq(30, 100, by=10), "th"))

    if (remove) {
        ordinal <- ""
    } else {
        ordinal <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
            "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth", 
            "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", 
            "nineteenth", "twentieth", "twenti first", "twenti second", "twenti third", 
            "twenti fith", "thirtieth", "fortieth", "fiftieth", "sixtieth", 
            "seventieth", "eightieth", "ninetieth", "hundredth")
    }
    if (num.paste) ordinal <- gsub("\\s+", "", ordinal)
    mgsub(symb, ordinal, text.var)
}
