## Tools to condense lists of vectors stored in dataframes for export to csv/xlsx
## Not currently exported
paste3 <- function(x, sep = ", "){
    sapply(x, paste, collapse = sep)
}

condense <- function(dataframe, ...) {
    whichlist <- sapply(dataframe, is.list)
    dataframe[, whichlist] <- sapply(dataframe[, whichlist], paste3, ...)
    dataframe
}

## poldat <- with(DATA, polarity(state, person))
## out1 <- merge(DATA, poldat$all, by.x=qcv(person, state), by.y=qcv(person, text.var))
## 
## library(xlsx)
## write.csv(x = condense(out1), file = "out1.csv")
