termco2mat <-
function (dataframe, drop.wc=TRUE, short.colnames=TRUE) {
    ind <- if(drop.wc) 1:2 else 1
    MAT <- as.matrix(dataframe[, -c(ind)])
    rownames(MAT) <- dataframe[, 1]
    if (short.colnames) {
        x <- gsub("term(", "", colnames(MAT), fixed=TRUE)
        colnames(MAT) <- gsub(")", "", x, fixed=TRUE)
    }
    return(MAT)
}

