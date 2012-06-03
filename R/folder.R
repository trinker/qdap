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
folder <-
function(folder.name = NULL) {
    FN <- if (is.null(folder.name)) {
        SS <- gsub(":", ".", substr(Sys.time(), 1, 19))
        paste(substr(SS, 1, 10), "  Time", substr(SS, 11, 19), sep = "")
    } else {
        as.character(substitute(folder.name))
    }
    x <- paste(getwd(), "/", FN, sep = "")
    dir.create(x)
    return(FN)
}
