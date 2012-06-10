incomplete.replace <-
function(text.var, en.dash = FALSE,
    em.dash = FALSE) {
    x <- gsub("\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|
        [.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+", "|", 
        scrubber(text.var))
    if (en.dash) {
        x <- gsub("[–]", "|", x)
    }
    if (em.dash) {
        x <- gsub("[—]", "|", x)
    }
    return(x)
}

incomp <- incomplete.replace