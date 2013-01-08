#' Denote Incomplete End Marks With "|"
#' 
#' Replaces incomplete sentence end marks (.., ..., .?, ..?, en \& em dash etc.)
#' with \code{"|"}.
#' 
#' @rdname incomplete.replace
#' @param text.var  The text variable.
#' @param scan.mode logical.  If TRUE only scans and reports incomplete sentences.
#' @return Returns a text variable (character sting) with incomplete sentence 
#' marks (.., ..., .?, ..?, en \& em dash etc. replaced with "|".  If scan mode 
#' is TRUE returns a data frame with incomplete sentence location.
#' @keywords incomplete-sentence
#' @export
#' @examples
#' \dontrun{
#' x <- c("the...",  "I.?", "you.", "threw..", "we?")
#' incomplete.replace(x)
#' incomp(x)
#' incomp(x, TRUE)
#' }
incomplete.replace <-
function(text.var, scan.mode = FALSE) {
  pat <- "\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|
        [.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+"
    if (scan.mode) {
      wid <- options()$width
      options(width = 10000)
      sel <- grepl(pat, scrubber(text.var))
      x <- data.frame(row.num = which(sel), 
          text = as.character(text.var[sel]))
      print(left.just(x, 2))
      options(width = wid)
    } else {
      x <- gsub(pat, "|", scrubber(text.var))
      return(x)
    }
}

#' @rdname incomplete.replace
#' @export
incomp <- incomplete.replace
