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
pollysyllable.sum <-
function(text) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        y <- as.data.frame(table(syllable.count(Trim(x))["syllables"]))
        z <- subset(y, as.numeric(as.character(Var1)) >= 3)
        j <- sum(z$Freq)
        return(j)
    }
    unlist(lapply(as.character(text), function(x) counter(x)))
}
