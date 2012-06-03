#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
colsplit2df <-
function(dataframe, column=1, orig.keep=FALSE, ...) {
    nc <- if (!is.numeric(column)) {
        match(column, names(dataframe))
    } else {
        column
    }
    ncols <- colSplit(dataframe[, column, drop=FALSE], ...)
    if(orig.keep) {
        cbind(dataframe[, nc, drop=FALSE], ncols, dataframe[, -nc])
    } else {
        cbind(ncols, dataframe[, -nc])
    }
}
