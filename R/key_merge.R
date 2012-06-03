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
key_merge <-
function(transcript.df, key.df, common.column, 
    defualt.arrange = TRUE) {
    DF <- merge(transcript.df, key.df, by = c(common.column, 
        common.column), incomparables = NA)
    if (defualt.arrange) {
        DF[, c(1, 3:ncol(DF), 2)]
    } else {
        DF
    }
}
