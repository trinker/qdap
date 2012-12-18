#' Remove Rows That Contain Markers
#' 
#' Remove rows from a data set that contain a given marker/term.
#' 
#' @param dataframe A dataframe object.
#' @param search.column Column name to search for markers/terms.
#' @param terms Terms/markers of the rows that are to be removed from the 
#' dataframe.  The term/marker must appear at the begining of the string and is 
#' case sensitive.
#' @return Creates a dataframe with the termed/markered rows removed.
#' @examples
#' rm_row(DATA, "person", c("sam", "greg"))
#' rm_row(DATA, 1, c("sam", "greg"))
#' rm_row(DATA, "state", c("Comp"))
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
