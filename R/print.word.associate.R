print.word.associate <-
function(word.associate) {
    y <- which(substring(names(word.associate), 
        1, 4) == "obs.")
    word.associate2 <- lapply(y, function(x) {
        rownames(word.associate[[x]]) <- NULL
        return(word.associate[[x]])
    })
    wid <- options()$width
    options(width = 10000)
    lapply(y, function(x) print(left.just(word.associate2[[x]], 2)))
    options(width = wid)
}
