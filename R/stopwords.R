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
stopwords <-
function(textString, stopwords = Top25Words) {  
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    } else {
        stopwords
    }
    SW <- function(textString, stopwords) {
        "%w/o%" <- function(x, y) x[!x %in% y]
        unlist(strsplit(tolower(Trim(textString)), 
            " ")) %w/o% tolower(Trim(stopwords))
    }
    sapply(textString, function(x) SW(x, Stopwords))
}
