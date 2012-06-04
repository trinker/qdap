colsplit2df <-
function(dataframe, column=1, orig.keep=FALSE, ...) {
    nc <- if (!is.numeric(column)) {
        match(column, names(dataframe))
    } else {
        column
    }
    ncols <- colSplit(dataframe[, column, drop=FALSE], ...)
    if(orig.keep) {
        cbind(dataframe[, nc, drop=FALSE], ncols, dataframe[, -nc])
    } else {
        cbind(ncols, dataframe[, -nc])
    }
}
