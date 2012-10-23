#' Removed Bracketted Text
#' 
#' Transcript Apply Removal of Brackets and Encased Text
#' 
#' @param text.var The text variable
#' @param bracket The type of bracket (and encased text) to remove.  This is one of 
#' the strings "curly", "square", "round", "angle" and "all".  
#' These strings correspond to: {, [, (, < or all four types.
#' @param missing Value to assign to empty cells.
#' @param names logical.  If TRUE the sentences are given as the names of the counts.
#' @return Retruns a vector of text with brackets removed.
#' @seealso \code[qdap]{\link{bracketXtract}}
#' @keywords bracket remove
#' @examples
#' examp2 <- examp2 <- structure(list(person = structure(c(1L, 2L, 1L, 3L), 
#'     .Label = c("bob", "greg", "sue"), class = "factor"), text = 
#'     c("I love chicken [unintelligible]!", 
#'     "Me too! (laughter) It's so good.[interupting]", 
#'     "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names = 
#'     c("person", "text"), row.names = c(NA, -4L), class = "data.frame")    
#' 
#' examp2                                                              
#' bracketX(examp2$text, 'square')  
#' bracketX(examp2$text, 'curly')  
#' bracketX(examp2$text)  
#'                                               
#' examp2                                              
#' bracketXtract(examp2$text, 'square')  
#' bracketXtract(examp2$text, 'curly')  
#' bracketXtract(examp2$text)  
#' 
#' paste2(bracketXtract(examp2$text, 'curly'), " ")
bracketX <-
function (text.var, bracket = "all", missing = NULL, names = FALSE) {
    X <- switch(bracket, 
        html = sapply(text.var, function(x) gsub("<.+?>", "", x)),
        angle = sapply(text.var, function(x) gsub("<.+?>", "", x)),
        square = sapply(text.var, function(x) gsub("\\[.+?\\]", "", x)), 
        round = sapply(text.var, function(x) gsub("\\(.+?\\)", "", x)), 
        curly = sapply(text.var, function(x) gsub("\\{.+?\\}", "", x)), 
        all = {
            P1 <- sapply(text.var, function(x) gsub("\\[.+?\\]", "", x))
            P1 <- sapply(P1, function(x) gsub("\\(.+?\\)", "", x))
            P1 <- sapply(P1, function(x) gsub("<.+?>", "", x))
            sapply(P1, function(x) gsub("\\{.+?\\}", "", x))
        }
    )
    X <- scrubber(gsub(" +", " ", X))
    if (!is.null(missing)) {
        X[X == ""] <- missing
    }
    if (!names) names(X) <- NULL
    X
}
