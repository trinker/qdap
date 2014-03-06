#' Condense Dataframe Columns 
#' 
#' Condense dataframe columns that are a list of vectors to a single vector of 
#' strings.
#' 
#' @param dataframe A dataframe with a column(s) that are a list of vectors.
#' @param sep A character string to separate the terms.
#' @return Returns a dataframe with condensed columns that can be wrote to 
#' csv/xlsx.
#' @seealso \code{\link[qdap]{mcsv_w}}
#' @export 
#' @examples
#' \dontrun{
#' library(qdap)
#' poldat <- with(DATA.SPLIT, polarity(state, person))
#' write.csv(x = condense(counts(poldat)), file = "foo.csv")
#' }
condense <- 
function(dataframe, sep = ", ") {
    whichlist <- sapply(dataframe, is.list)
    if (sum(whichlist) > 0) {
        dataframe[, whichlist] <- sapply(dataframe[, whichlist], paste3, sep = sep)
    }
    dataframe
}

paste3 <-
function(x, sep = ", "){
    paste(sapply(x, paste, collapse = sep), collapse = sep)
}

