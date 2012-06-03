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
g <-
function(x, k = 100){
    n <- length(x)
    if (sum(x) < k) {
        ans <- rep(NA, n)
        return(factor(ans))
    }
    breaks <- c()
    repeat{
        spot <- suppressWarnings(min(which(cumsum(x) >= k)))
        if (!is.finite(spot)) {
            break 
        }
        x <- x[-c(1:spot)]
        breaks <- c(breaks, spot)
    }
    ans <- rep(NA, n)
    groups <- paste("sel_", rep(1:(length(breaks)), breaks), sep = "")
    ans[1:length(groups)] <- groups
    return(factor(ans))
}
