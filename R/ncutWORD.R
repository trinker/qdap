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
ncutWORD <-
function(freqWORDS.DF, cut.n = 15) {
    if (nrow(freqWORDS.DF) > cut.n) {
        subset(freqWORDS.DF, FREQ >= freqWORDS.DF[cut.n, "FREQ"])
    } else {
        freqWORDS.DF
    }
}
