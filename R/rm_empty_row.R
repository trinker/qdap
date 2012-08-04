rm_empty_row <-
function(dataframe) {
    x <- paste2(dataframe, sep="")
    x <- gsub("\\s+", "", x)
    ind <- x != ""
    return(dataframe[ind, ] )
}
