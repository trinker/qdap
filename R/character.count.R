character.count <-
function(text, by = "row", missing = NA, 
    apostrophe = TRUE, digit.remove = TRUE) {
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
