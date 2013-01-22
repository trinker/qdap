#' Dataframe Viewing
#' 
#' \code{htruncdf} - Convenience function to view the head of a truncated 
#' dataframe.
#' 
#' @param dataframe A data.frame object.
#' @param n Number of rows to display.
#' @param width The width of the columns to be displayed.
#' @param end The last character to be displayed (width).
#' @param begin The first character to be displayed (width).
#' @param \ldots Other arguments passed to \code{\link[qdap]{htruncdf}}.
#' @rdname data_viewing
#' @return \code{htrundf} - returns n number of rows of a truncated dataframe.
#' @seealso \code{\link[utils]{head}}
#' @export
#' @examples
#' \dontrun{
#' htruncdf(raj)
#' htruncdf(raj, 20)
#' htruncdf(raj, ,20)
#' truncdf(raj)
#' truncdf(raj, 40)
#' qview(raj)
#' qview(CO2)
#' }
htruncdf <-
function(dataframe, n=10, width=10, ...) {
    head(truncdf(as.data.frame(dataframe), width), n = n, ...)
}

#' Truncated Dataframe Viewing
#' 
#' \code{truncdf} - Convenience function to view a truncated dataframe. 
#' 
#' @return \code{trundf} - returns a truncated dataframe.
#' @rdname data_viewing
#' @export
truncdf <- 
function(dataframe, end=10, begin=1) {
    x <- as.data.frame(dataframe)
    DF <- data.frame(lapply(x, substr, begin, end), check.names=FALSE)
    names(DF) <- substring(names(DF), begin, end)
    DF
}

#' Summary Dataframe Viewing
#' 
#' \code{qview} - Convenience function to view a summary and head of a dataframe.
#' 
#' @return \code{qview} - returns a dataframe head with summary statistics.
#' @rdname data_viewing
#' @export
qview <-
function(dataframe, ...){
    x <- as.character(substitute(dataframe))
    y <- paste(rep("=", 72), collapse="")   
    z <- paste("nrow = ",nrow(dataframe), "          ncol = ",
        ncol(dataframe), "           ", x, collapse="")
    cat(paste(y, z, y, sep = "\n")); cat("\n")
    return(htruncdf(dataframe, ...))
}