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
prop <-
function(mat, digits = 2, percent = FALSE) {
    per <- if (percent) 100 else 1
    daf <- data.frame(mat)
    daf <-sapply(seq_along(daf), function(x) round(per*(mat[, x]/sum(mat)), 
        digits = digits))
    mat2 <- as.matrix(daf)
    colnames(mat2) <- colnames(mat)
    return(mat2)
}
