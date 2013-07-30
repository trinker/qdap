#' Replace Blanks in a dataframe
#' 
#' Replaces blank (empty) cells in a dataframe.  Generally, for internal use.
#' 
#' @param dataframe A dataframe with blank (empty) cells.
#' @param missing Value to replace empty cells with.
#' @return Returns a data frame with blank spaces replaced.
#' @seealso \code{\link[qdap]{rm_row}}
#' @export
#' @examples
#' \dontrun{
#' set.seed(15)
#' dat <- data.frame(matrix(sample(c(month.abb[1:4], ""), 50, TRUE), 
#'     10, byrow = TRUE), stringsAsFactors = FALSE)
#' 
#' dat
#' blank2NA(dat)
#' }
blank2NA <- 
function (dataframe, missing = NA) {
    dataframe[dataframe == ""] <- missing
    dataframe
}
