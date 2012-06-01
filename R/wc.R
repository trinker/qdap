wc <-
function(text, by = "row", missing = NA, 
    digit.remove = TRUE) {
    txt <- sapply(as.character(text), function(x) {
            ifelse(is.na(x)|is.null(x), "", x)
        }
    )
    OP <- switch(by, 
         all = {length(unblanker(unlist(word.split(reducer(unlist(strip(
                   paste(as.character(txt), collapse = " "), 
                   digit.remove = digit.remove)))))))}, 
         row = {sapply(txt, function(x) length(unblanker(unlist(word.split(
                   reducer(unlist(strip(as.character(x), digit.remove = 
         digit.remove))))))))}
    )
    ifelse(OP==0, missing, OP)
}
