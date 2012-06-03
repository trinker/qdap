#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
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
