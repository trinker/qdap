common.list <-
function(LIS, overlap = "all", equal.or = "more"){
    OL <- if(overlap=="all") length(LIS) else overlap
    LIS <- sapply(LIS, unique)
    DF <- as.data.frame(table(unlist(LIS)),  stringsAsFactors = FALSE)
    names(DF) <- c("word", "freq")
    DF <- DF[order(-DF$freq, DF$word), ]
    DF <- switch(equal.or,
        equal = DF[DF$freq == OL, ],
        greater = DF[DF$freq > (OL - 1), ],
        more = DF[DF$freq > (OL - 1), ],
        less = DF[DF$freq < (OL + 1), ])
    rownames(DF) <- 1:nrow(DF)
    return(DF)
}
