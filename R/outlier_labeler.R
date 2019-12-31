#' Locate Outliers in Numeric String
#' 
#' Locate and label possible outliers in a string.
#' 
#' @param x A numeric vector.
#' @param standardize logical.  If \code{TRUE} scales the vector first.
#' @param \ldots Other arguments passed to \code{\link[base]{scale}}.
#' @return Returns a matrix (one column) of possible outliers coded as 
#' \code{"3sd"}, \code{"2sd"} and \code{"1.5sd"}, corresponding to >= to 3, 2, 
#' or 1.5 standard deviations.
#' @seealso \code{\link[base]{scale}}
#' @export
#' @examples
#' \dontrun{
#' outlier_labeler(mtcars$hp)[20:32]
#' by(mtcars$mpg, mtcars$cyl, outlier_labeler)
#' tapply(mtcars$mpg, mtcars$cyl, outlier_labeler)
#' }
outlier_labeler <- function(x, standardize = TRUE, ...){
    if (standardize) {
        x <- scale(x, ...)
    }
    ifelse(abs(x) >= 3, "3sd", ifelse(abs(x) >= 2 & abs(x) < 3, "2sd", 
        ifelse(abs(x) >= 1.5 & abs(x) < 2, "1.5sd", "-")))
}
