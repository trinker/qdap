strip <-
function(x, digit.remove = TRUE, apostrophe.remove = FALSE){
    strp <- function(x, digit.remove, apostrophe.remove){
        x2 <- Trim(tolower(gsub(".*?($|'|[^[:punct:]]).*?", 
            "\\1", as.character(x))))
        x2 <- if(apostrophe.remove) gsub("'", "", x2) else x2
        ifelse(digit.remove==TRUE, gsub("[[:digit:]]", "", x2), x2)
    }
    unlist(lapply(x, function(x) Trim(strp(x =x, 
        digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove)) ))
}
