#' Replace Blanks in Data Frame
#' 
#' Replaces blank (empty) cells in a dataframe.  generally, for internal use.
#' 
#' @param A dataframe with blank (empty) cells.
#' @param missing Value to replace empty cells with.
#' @return Returns a dataframe with blank spaces replaced.
#' @seealso \code{\link[qdap]{unblanker}}
#' @export
#' @examples
#' \dontrun{
#' dat <- data.frame(matrix(sample(c(1:4, ""), 50, TRUE), 
#'     10, byrow = TRUE), stringsAsFactors = FALSE)
#' dat
#' blank2NA(dat)
#' }
blank2NA <-
function(dataframe, missing=NA){
    cn <- which(sapply(dataframe, is.character))
    FUN <- function(da) {
        if (is.character(da)) da <- Trim(da)
        da[da == ""] <- missing
        names(da) <- NULL
        return(da)
    }
    DF <- data.frame(apply(dataframe, 2, FUN), check.names=FALSE)
    invisible(sapply(cn, function(i) {
        DF[, i] <<- as.character(DF[, i])
    }))
    DF
}
