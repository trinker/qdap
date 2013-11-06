## rbinds and fills empties with zero
## http://stackoverflow.com/a/19804707/1000343
rbind_qdap <- function(x) {
    allnames <- unique(unlist(lapply(x, names)))
    data.frame(do.call(rbind, lapply(x, function(df) {
        not <- allnames[!allnames %in% names(df)]
        df[, not] <- 0
        df
    })), row.names = NULL)

}