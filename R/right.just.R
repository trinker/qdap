right.just <-
function(dataframe){
    NAMES <- Trim(names(dataframe))
    x <- data.frame(sapply(dataframe, Trim))
    names(x) <- NAMES
    return(x)
}
