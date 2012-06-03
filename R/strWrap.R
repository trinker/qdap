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
strWrap <-
function(text = "clipboard", width = 70) {
    x <- if (text == "clipboard") {
         paste(readClipboard(), collapse=" ")
    } else {
        text
    }
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", x))
    x <- strwrap(x, width = width)
    writeClipboard(x, format = 1)
    writeLines(strwrap(x, width = width))
}
