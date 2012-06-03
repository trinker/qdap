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
colSplit <-
function(column, name.sep = "&", col.sep = "."){
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call('rbind', svar))
    if (length(unlist(strsplit(names(column), 
        name.sep, fixed = TRUE))) > 1){
        cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
        names(svar) <- cn
    } 
    return(svar)
}
