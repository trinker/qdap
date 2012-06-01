blank2NA <-
function(dataframe, missing=NA){
    cn <- which(sapply(dataframe, is.character))
    FUN <- function(da) {
        if (is.character(da)) da <- Trim(da)
        da[da==""] <- missing
        names(da) <- NULL
        return(da)
    }
    DF <- data.frame(apply(dataframe, 2, FUN), check.names=FALSE)
    sapply(cn, function(i) {DF[, i] <<- as.character(DF[, i])})
    return(DF)
}
