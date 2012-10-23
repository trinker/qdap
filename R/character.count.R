#' Count Number of Characters
#' 
#' Transcript Apply Character Counts
#' 
#' @param text.var The text variable
#' @param by an optional character string giving a method for counting words.  
#' This is one of the strings ``row'' or ``all''.
#' @param missing Value to insert for missing values (empty cells).
#' @param apostrophe logical.  If TRUE apostrophes will be counted in the character count.
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @return Returns a character count by row or total.
#' @seealso \code[qdap]{\link{word.count}}
#' @keywords character count
#' @examples
#' character.count(DATA$state)
#' character.count(DATA$state, by= "all")
#' sum(character.count(DATA$state))
character.count <-
function(text.var, by = "row", missing = NA, apostrophe = TRUE, 
    digit.remove = TRUE) {
    text2 <- if (apostrophe == TRUE) {
        text
    } else {
        gsub("'", "", text, fixed = TRUE)
    }
    chara <- function(x) {
        y <- unlist(strsplit(strip(x, digit.remove = digit.remove), 
            NULL))
        z <- subset(y, y != " ")
        length(z)
    }
    OP <- switch(by, 
           all = chara(paste(unlist(text2), collapse = "  ")), 
           row = unlist(lapply(text2, function(x) chara(x)))
          )
    ifelse(OP == 0, missing, OP)
}
