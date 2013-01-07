#' Remove Rows That Contain Markers
#' 
#' \code{rm_row} - Remove rows from a data set that contain a given marker/term.
#' 
#' @param dataframe A dataframe object.
#' @param search.column Column name to search for markers/terms.
#' @param terms Terms/markers of the rows that are to be removed from the 
#' dataframe.  The term/marker must appear at the begining of the string and is 
#' case sensitive.
#' @return \code{rm_row} - returns a dataframe with the termed/markered rows 
#' removed.
#' @rdname rm_row
#' @export
#' @examples
#' \dontrun{
#' #rm_row EXAMPLE:
#' rm_row(DATA, "person", c("sam", "greg"))
#' rm_row(DATA, 1, c("sam", "greg"))
#' rm_row(DATA, "state", c("Comp"))
#' 
#' #rm_empty_row EXAMPLE:
#' x <- matrix(rep(" ", 4), ncol =2)
#' dat <- DATA[, c(1, 4)]
#' colnames(x) <- colnames(dat)
#' (dat <- data.frame(rbind(dat, x)))
#' rm_empty_row(dat)
#' }
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

#' Remove Empty Rows in a Data Frame
#' 
#' \code{rm_empty_row} - Removes the empty rows of a data set that are common in 
#' reading in data (default method in \code{read.transcript}).
#' 
#' @return \code{rm_empty_row} - returns a dataframe with empty rows removed.
#' @rdname rm_row
#' @export
rm_empty_row <-
function(dataframe) {
    x <- paste2(dataframe, sep="")
    x <- gsub("\\s+", "", x)
    ind <- x != ""
    return(dataframe[ind,  ,drop = FALSE] )
}
