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
Search <-
function(term, dataframe, column.name, variation = 0.02, ...) {
    te <- as.character(substitute(term))
    cn <- as.character(substitute(column.name))
    HUNT <- agrep(te, dataframe[, cn], ignore.case = TRUE, 
        max.distance = variation, ...)
    dataframe[c(HUNT), ]
}
