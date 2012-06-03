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
sentFind <-
function(dataframe, n, margin = 2, text.col = "all") {
    surround <- function(N, mar, DF) {
        x <- c((N - mar:1), N, (N + 1:mar))
        x[x > 0 & x < nrow(DF)]
    }
    z <- if (margin == 0) {
        n
    } else {
        surround(N = n, mar = margin, DF = dataframe)
    }
    if (text.col == "all" | is.null(text.col)) {
        dataframe[z, ]
    } else {
        as.character(dataframe[z, text.col])
    }
}
