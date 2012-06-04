htruncDF <-
function(dataframe, n=10, width=10) {
    x <- head(truncDF(dataframe, width),n=n)
    colnames(x) <- as.character(colnames(dataframe))
    return(x)
}
