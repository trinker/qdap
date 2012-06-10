incomplete.replace <-
function(text.var) {
    x <- gsub("\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|
        [.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+", "|", 
        scrubber(text.var))
    return(x)
}

incomp <- incomplete.replace