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
tester <-
function(string, code, type="logical"){
    C <- as.character(substitute(code))
    test <- function(S, x) any(S==x)
    x <- sapply(string, function(x)test(x, C))
    if (type=="logical") {
        x 
    } else {
        if (type=="numeric") {
            which(x)
        } else {
            stop("type must be logical or numeric")
        }
    }
}
