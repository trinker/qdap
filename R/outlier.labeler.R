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
outlier.labeler <- function(x, standardize=TRUE){
    if (standardize) x <- scale(x)
    y <- ifelse(abs(x) >= 3, "3sd", 
        ifelse(abs(x) >= 2 & abs(x) < 3, "2sd", 
        ifelse(abs(x) >= 1.5 & abs(x) < 2, 
        "1.5sd", "-")))
    return(y)
}