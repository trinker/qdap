endf <-
function(dataframe, text.var, report=TRUE){
    tx <- scrubber(dataframe[, as.character(substitute(text.var))])
    nc <- nchar(tx)
    keep <- substring(tx, nc, nc) != "|"
    if (report) {
        cat(sum(!keep, na.rm=TRUE), "incomplete sentence items removed\n")
    }
    return(dataframe[keep, ])
}
