#' Bracket Parsing
#' 
#' \code{bracketX} - Apply bracket removal to character vectors.
#' 
#' @param text.var The text variable
#' @param bracket The type of bracket (and encased text) to remove.  This is one of 
#' the strings \code{"curly"}, \code{"square"}, \code{"round"}, \code{"angle"} 
#' and \code{"all"}.  These strings correspond to: \{, [, (, < or all four types.
#' @param missing Value to assign to empty cells.
#' @param names logical.  If TRUE the sentences are given as the names of the counts.
#' @return bracketX returns a vector of text with brackets removed.
#' @rdname bracketX
#' @references \url{http://stackoverflow.com/questions/8621066/remove-text-inside-brackets-parens-and-or-braces}
#' @keywords bracket-remove, parenthesis, bracket, curly-braces
#' @export
#' @examples
#' examp2 <- examp2 <- structure(list(person = structure(c(1L, 2L, 1L, 3L), 
#'     .Label = c("bob", "greg", "sue"), class = "factor"), text = 
#'     c("I love chicken [unintelligible]!", 
#'     "Me too! (laughter) It's so good.[interupting]", 
#'     "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names = 
#'     c("person", "text"), row.names = c(NA, -4L), class = "data.frame")    
#' 
#' examp1                                                              
#' bracketX(examp2$text, 'square')  
#' bracketX(examp2$text, 'curly')  
#' bracketX(examp2$text)  
#'                                               
#' examp2                                              
#' bracketXtract(examp2$text, 'square')  
#' bracketXtract(examp2$text, 'curly')  
#' bracketXtract(examp2$text)  
#' bracketXtract(examp2$text, with = TRUE)
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

#' bracketXtract
#' 
#' \code{bracketXtract} - Apply bracket extraction to character vectors.
#' 
#' @rdname bracketX
#' @param with logical.  If TRUE returns the brackets and the bracketted text.
#' @return bracketXtract returns a list of vectors of bracketed text.
#' @author  Martin Morgan and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @export
bracketXtract <-
function(text.var, bracket = "all", with = FALSE){   
    br <- bracket
    br <- ifelse(br=="round", "(", 
        ifelse(br=="square", "[", 
        ifelse(br=="curly", "{",
        ifelse(br=="html", "<",
        ifelse(br=="angle", "<", br)))))
    left <- if ("all" == br) {
        "\\(|\\{|<|\\["
    } else {
        sprintf("\\%s", br)
    }
    map <- c(`\\(`="\\)", `\\[`="\\]", `\\{`="\\}",
             `\\<`="\\>", `\\(|\\{|<|\\[`="\\)|\\}|\\>|\\]")
    fmt <- if (with==TRUE) {
        "(%s).*?(%s)"
    } else {
        "(?<=%s).*?(?=%s)"
    }
    re <- sprintf(fmt, left, map[left])
    if(length(text.var)==1){
        unlist(regmatches(text.var, gregexpr(re, text.var, perl=TRUE)))
    }else{  
        regmatches(text.var, gregexpr(re, text.var, perl=TRUE)) 
    }
}