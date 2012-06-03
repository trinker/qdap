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
termco2mat <-
function (dataframe, drop.wc=TRUE, short.colnames=TRUE) {
    ind <- if(drop.wc) 1:2 else 1
    MAT <- as.matrix(dataframe[, -c(ind)])
    rownames(MAT) <- dataframe[, 1]
    if (short.colnames) {
        x <- gsub("term(", "", colnames(MAT), fixed=TRUE)
        colnames(MAT) <- Trim(gsub(")", "", x, fixed=TRUE))
    }
    return(MAT)
}

