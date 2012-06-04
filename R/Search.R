Search <-
function(term, dataframe, column.name, variation = 0.02, ...) {
    te <- as.character(substitute(term))
    cn <- as.character(substitute(column.name))
    HUNT <- agrep(te, dataframe[, cn], ignore.case = TRUE, 
        max.distance = variation, ...)
    dataframe[c(HUNT), ]
}
