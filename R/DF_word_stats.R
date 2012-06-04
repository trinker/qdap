#' Transcript Apply Descriptive Word Statistics
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param digit.remove %% ~~Describe \code{digit.remove} here~~
#' @param apostrophe.remove %% ~~Describe \code{apostrophe.remove} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text.var, digit.remove = FALSE, apostrophe.remove = FALSE) 
#' {
#'     text <- as.character(text.var)
#'     DF <- na.omit(data.frame(text.var = text, stringsAsFactors = FALSE))
#'     DF$n.sent <- 1:nrow(DF)
#'     DF$word.count <- word.count(DF$text.var, missing = 0, digit.remove = digit.remove)
#'     DF$character.count <- character.count(DF$text.var, apostrophe = apostrophe.remove, 
#'         digit.remove = digit.remove)
#'     DF$word.count <- word.count(DF$text.var, missing = 0, digit.remove = digit.remove)
#'     DF <- data.frame(DF, combo_syllable.sum(DF$text.var))
#'     DF <- DF[, c("text.var", "n.sent", "word.count", "character.count", 
#'         "syllable.count", "polysyllable.count")]
#'     DF <- transform(DF, char2word.ratio = round(character.count/word.count, 
#'         digits = 3), syl2word.ratio = round(syllable.count/word.count, 
#'         digits = 3), polysyl2word.ratio = round(polysyllable.count/word.count, 
#'         digits = 3))
#'     punctuation <- function(x) substr(x, nchar(x), nchar(x))
#'     DF$end.mark <- unlist(lapply(DF$text.var, punctuation))
#'     rownames(DF) <- 1:nrow(DF)
#'     DF <- DF[order(DF$n.sent), ]
#'     return(DF)
#'   }
#' 
DF_word_stats <-
function(text.var, digit.remove = FALSE, 
    apostrophe.remove = FALSE) {
    text <- as.character(text.var)
    DF <- na.omit(data.frame(text.var = text, 
        stringsAsFactors = FALSE))
    DF$n.sent <- 1:nrow(DF)
    DF$word.count <- word.count(DF$text.var, missing = 0, 
        digit.remove = digit.remove)
    DF$character.count <- character.count(DF$text.var, 
        apostrophe = apostrophe.remove, digit.remove = digit.remove)
    DF$word.count <- word.count(DF$text.var, missing = 0, 
        digit.remove = digit.remove)
    DF <- data.frame(DF, combo_syllable.sum(DF$text.var))
    DF <- DF[, c("text.var", "n.sent", "word.count", "character.count",
        "syllable.count",  "polysyllable.count") ]
    DF <- transform(DF, char2word.ratio = 
        round(character.count/word.count, digits=3),
        syl2word.ratio = round(syllable.count/word.count, digits=3),
        polysyl2word.ratio = round(polysyllable.count/word.count, digits=3))
    punctuation <- function(x) substr(x, nchar(x), nchar(x))
    DF$end.mark <- unlist(lapply(DF$text.var, punctuation))
    rownames(DF) <- 1:nrow(DF)
    DF <- DF[order(DF$n.sent),]  
    return(DF)
}
