#' Make Plural (or Verb to Singular) Versions of Words
#' 
#' Add -s, -es, or -ies to words.
#' 
#' @param x A vector of words to make plural.
#' @param keep.original logical.  If \code{TRUE} the original words are kept in 
#' the return vector.
#' @return Returns a vector of plural words.
#' @keywords plural
#' @export
#' @examples 
#' set.seed(10)
#' add_s(sample(GradyAugmented, 10))
#' set.seed(10)
#' add_s(sample(GradyAugmented, 10), FALSE)
add_s <- function(x, keep.original = TRUE) {
    ends <- qdapRegex::pastex(c("s$", "x$", "ch$", "sh$"))
    pluralify <- ifelse(grepl(ends, x), "es", "s")
    
    c(if (keep.original){x}, gsub("ys$", "ies", paste0(x, pluralify)))
}

