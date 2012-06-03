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
qda.handler <-
function(x) {
    if (!is.null(comment(x))) {
        if (comment(x) %in% "freqList") {
            return(freqTab2words(x))
        } else {
            if (!comment(x) %in% "bagOwords") {
                stop("Must be a qdap object or a vector of raw words")
            } else {
                return(x)
            }
        }
    } else {
        if (is.vector(x)) {
            warning ("Not a qdap object.")
            return(x)
        } else {
            stop("Must be a qdap object or a vector of raw words")
        }
    }
}
