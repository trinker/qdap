#' Sentence End marks
#' 
#' Grab the sentence end marks for a transcript.  This can be useful to 
#' categorize based on sentence type.
#' 
#' @param text.var The text variable.        
#' @return Returns a character vector of qdap end marks for each sentence.  
#' End marks include:
#' \item{"."}{Declarative sentence.} 
#' \item{"?"}{Question sentence.} 
#' \item{"!"}{Exclamatory sentence.} 
#' \item{"|"}{Incomplete sentence.} 
#' \item{"*."}{Imperative-declarative sentence.} 
#' \item{"*?"}{Imperative-question sentence (unlikely to occur)} 
#' \item{"*!"}{Imperative-exclamatory sentence.} 
#' \item{"*|"}{Imperative-incomplete sentence.} 
#' \item{"no.em"}{No end mark.}
#' \item{"blank"}{Empty cell/NA.} 
#' @keywords end-mark
#' @export
#' @examples
#' \dontrun{
#' end_mark(DATA$state)
#' end_mark(mraja1spl$dialogue)
#' ques <- mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] #grab questions
#' htruncdf(ques)
#' non.ques <- mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] #non questions
#' htruncdf(non.ques, 20)
#' ques.per <- mraja1spl[end_mark(mraja1spl$dialogue) %in% c(".", "?"), ] #grab ? and .
#' htruncdf(ques.per, 20)
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
