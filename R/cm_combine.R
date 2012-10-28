#I believe this combine's codes into one and optionally eliminates the old
cm_combine <-
function(dataframe, combined.columns, elim.old = FALSE) { 
    combo <- function(dat, comb, new.name, elim.old) {
        DF <- data.frame(dat, as.numeric(rowSums(dat[, comb]) > 0))
        names(DF)[ncol(DF)] <- new.name
        if (elim.old) {
            DF <- DF[, !names(DF) %in% comb]
        }
        return(DF)
    }
    lapply(seq_along(combined.columns), function(i) {
        dataframe <<- combo(dat = dataframe, comb = combined.columns[[i]], 
            new.name = names(combined.columns)[i], elim.old = elim.old)
        }
    )
    return(dataframe)
}
