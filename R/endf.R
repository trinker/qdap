endf <-
function(dataframe, text.var, warning.report=TRUE, which.mode = FALSE){
    tx <- scrubber(dataframe[, as.character(substitute(text.var))])
    nc <- nchar(tx)
    keep <- substring(tx, nc, nc) != "|"
    if (which.mode) {
        return(list("NOT" = keep, "INC" = !keep))
    } else {
        if (warning.report) {
            warning(sum(!keep, na.rm=TRUE), 
            " incomplete sentence items removed\n")
        }
        return(dataframe[keep, ])
    }
}
