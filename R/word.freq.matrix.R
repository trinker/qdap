word.freq.matrix <-
function(wfdf = NULL, text.var = NULL, 
    grouping.var = NULL, stopwords = NULL, digits = 2){
    if (!is.null(wfdf)) {
        if (comment(wfdf) == "t.df") {
            wfdf <- wfdf
        } else {
            if (comment(wfdf) == "m.df") { 
                wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
            } else {
                stop("Object must be a raw word frequency data frame")
            }
        }

        x2 <- wfdf[, -1]
        rownames(x2) <- wfdf[, 1]
        x2 <- as.matrix(x2)
    } else {
        if (!is.null(text.var) & ! is.null(grouping.var)) {
            wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
                 stopwords = stopwords) 
            x2 <- wfdf[, -1]
            rownames(x2) <- wfdf[, 1]
            x2 <- as.matrix(x2)
        } else {
            stop ("must specify both text.var & grouping var or wfdf")
        }
    }
    comment(x2) <- "true.matrix"
    return(x2)
}
