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
word.freq.matrix <-
function(wfdf = NULL, text.var = NULL, 
    grouping.var = NULL, stopwords = NULL, digits = 2){
    if (!is.null(wfdf)) {
        if (comment(wfdf) == "t.df") {
            wfdf <- wfdf
        } else {
            if (comment(wfdf) == "m.df") { 
                wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
            } else {
                stop("Object must be a raw word frequency data frame")
            }
        }

        x2 <- wfdf[, -1]
        rownames(x2) <- wfdf[, 1]
        x2 <- as.matrix(x2)
    } else {
        if (!is.null(text.var) & ! is.null(grouping.var)) {
            wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
                 stopwords = stopwords) 
            x2 <- wfdf[, -1]
            rownames(x2) <- wfdf[, 1]
            x2 <- as.matrix(x2)
        } else {
            stop ("must specify both text.var & grouping var or wfdf")
        }
    }
    comment(x2) <- "true.matrix"
    return(x2)
}

wfm <- word.freq.matrix 
