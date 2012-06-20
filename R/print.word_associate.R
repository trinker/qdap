print.word_associate <-
function(word_associate) {
    y <- which(substring(names(word_associate), 
        1, 4) == "obs.")
    word.associate2 <- lapply(y, function(x) {
        rownames(word_associate[[x]]) <- NULL
        return(word_associate[[x]])
    })
    wid <- options()$width
    options(width = 10000)
    lapply(seq_along(word.associate2), 
        function(x) print(left.just(word.associate2[[x]], 2)))
    options(width = wid)
}
