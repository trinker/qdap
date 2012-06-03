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
termco.c <-
function(termco.d.object, combined.columns, new.name, 
    zero.replace = 0, elim.old = TRUE){     
    x <- termco.d.object$raw
    y <- termco.d.object$prop
    if (!is.numeric(combined.columns)){
        combined.columns <- which(names(x) %in% combined.columns)
    }
    x <- transform(x, new.name = rowSums(x[, combined.columns]), 
        check.names=FALSE)
    names(x)[length(x)] <- names(y)[length(y)] <- new.name
    y <- transform(y, new.name = rowSums(y[, combined.columns]), 
        check.names=FALSE)
    z <- if (elim.old) {
        seq_along(x)[!seq_along(x) %in% combined.columns] 
    } else {
        seq_along(x)
    }
    x2 <- replacer(x, with = zero.replace)[, z]
    y2 <- replacer(y, with = zero.replace)[, z]
    DF <- replacer(termco.rnp(x, y), "0(0)", with = zero.replace)[, z]
    o <- list(raw = x2, prop = y2, rnp = DF)
    class(o) <- "termco_c"
    return(o)
}
