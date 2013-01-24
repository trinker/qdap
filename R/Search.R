#' Search Columns of a Data Frame 
#' 
#' Find terms located in columns of a data frame.
#' 
#' @param dataframe A dataframe object to search.
#' @param term A character vector term to search for.
#' @param column.name Optional column of the data frame to search (character 
#' name or integer index).
#' @param max.distance Maximum distance allowed for a match. Expressed either as 
#' integer, or as a fraction of the pattern length times the maximal 
#' transformation cost (will be replaced by the smallest integer not less than 
#' the corresponding fraction).
#' @param \ldots Other arguments passed to \code{agrep}.
#' @return Returns the rows of the data frame that match the search term.
#' @keywords search
#' @export
#' @examples
#' \dontrun{
#' SampDF <- data.frame("islands"=names(islands)[1:32],mtcars)
#' 
#' Search(SampDF, "Cuba", "islands")
#' Search(SampDF, "New", "islands")
#' Search(SampDF, "Ho")
#' Search(SampDF, "Ho", max.distance = 0)
#' Search(SampDF, "Axel Heiberg")
#' Search(SampDF, 19) #too much tolerance in max.distance
#' Search(SampDF, 19, max.distance = 0)
#' Search(SampDF, 19, "qsec", max.distance = 0)
#' }
Search <-
function(dataframe, term, column.name = NULL, max.distance = 0.02, ...) {
    cn <- column.name
    if (!is.null(column.name)) {
        HUNT <- agrep(term, dataframe[, cn], ignore.case = TRUE, 
            max.distance = max.distance, ...)
    } else {
        ser <- invisible(lapply(dataframe, function(x) {
            agrep(term, x, ignore.case = TRUE, max.distance = max.distance, ...)
        }))
        ser2 <- sort(unlist(ser))
        names(ser2) <- NULL
        HUNT <- unique(ser2)
    }
    dataframe[HUNT, ]
}