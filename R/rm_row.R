rm_row <-
function(dataframe, search.column, terms) {
    FUN <- function(dat, sc, term) {
        dat[substring(dat[, sc], 1, nchar(term)) != term, ]
        
    }
    DF <- dataframe
    lapply(terms, function(x) {
            DF <<- FUN(DF, search.column, x)
        }
    )
    return(DF)
}
