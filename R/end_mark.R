#' Sentence Endmarks
#' 
#' Grab the sentence endmarks for a transcript.  This can be useful to 
#' 
#' @param text.var The text variable.        
#' @return Returns a character vector of qdap endmarks for each sentence.  
#' Endmarks include:
#' \item{"."}{Declarative sentence.} 
#' \item{"?"}{Question sentence.} 
#' \item{"!"}{Exclamatory sentence.} 
#' \item{"|"}{Incomplete sentence.} 
#' \item{"*."}{Imperative-declarative sentence.} 
#' \item{"*?"}{Imperative-question sentence (unlikely to occur)} 
#' \item{"*!"}{Imperative-exclamatory sentence.} 
#' \item{"*|"}{Imperative-incomplete sentence.} 
#' \item{"no.em"}{No endmark.}
#' \item{"blank"}{Empty cell/NA.} 
#' @keywords endmark
#' @export
#' @examples
#' \dontrun{
#' end_mark(DATA$state)
#' end_mark(mraja1spl$dialogue)
#' mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] #grab questions
#' mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] #non questions
#' mraja1spl[end_mark(mraja1spl$dialogue) %in% c(".", "?"), ] #grab ? and .
#' }
end_mark <- function(text.var) {
    text.var <-  as.character(text.var)
    if (is.dp(text.var=text.var)){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    y <- nchar(text.var)
    last1 <- substring(text.var, y)
    last2 <- substring(text.var, y-1)
    last1[last2 == "*."] <- "*."
    last1[last2 == "*?"] <- "*?"
    last1[last2 == "*!"] <- "*!"
    last1[last2 == "*|"] <- "*|"
    last1[!last1 %in% c("*.", "*?", "*!", "*|", ".", "?", "!", "|")] <- "no.em"
    last1[is.na(text.var)] <- "blank"
    last1
}