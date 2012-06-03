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
affix <-
function(char.string, n.char, affix="suffix") {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    ender <- function(x, s) {
        nc <- nchar(x)
        substring(x, (nc - s + 1), nc)
    }
    beginer <- function(x, s) substring(x, 1, s)
    if (affix == "suffix") {
        sapply(Trim(as.character(char.string)), function(x) ender(x, 
            n.char))
    } else {
        if (affix == "prefix") {
            sapply(Trim(as.character(char.string)), function(x) beginer(x, 
                n.char))
        } else {
            cat("\nWARNING:\nIncorrect affix argument!\n\n")
        }
    }
}
