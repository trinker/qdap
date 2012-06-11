endf <-
function(dataframe, text.var, warning.report=TRUE, which.mode = FALSE){
    tx <- scrubber(dataframe[, as.character(substitute(text.var))])
    nc <- nchar(tx)
    keep <- substring(tx, nc, nc) != "|"
    if (which.mode) {
        return(list("NOT" = keep, "INC" = !keep))
    } else {
        nrp <- sum(!keep, na.rm=TRUE)
        if (warning.report & nrp > 0) {
            warning(nrp, 
            " incomplete sentence items removed\n")
        }
        return(dataframe[keep, ])
    }
}
