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
qda.capitalizer <-
function(qda.obj, caps.list=NULL){
    CAPPER <- function(Y, caps.list) {
        Y[, 1] <- sapply(Y[, 1], function(x) capitalizer(x, caps.list), 
            USE.NAMES = FALSE)
        Y
    }
    CAPPER2 <- function(Y, caps.list) {
        Y <- sapply(Y, function(x) capitalizer(x, caps.list), USE.NAMES = FALSE)
        Y
    }
    if (is.null(comment(X))) {
            z <- lapply(qda.obj, function(x) CAPPER2(x, caps.list=caps.list))
            return(z, warning("not a qda object"))
    } else{
        if (comment(X)%in%c("fwl", "fswl", "rfswl")) {
            z <- lapply(qda.obj, function(x) CAPPER(x, caps.list=caps.list))
            return(z)
        } else {
            if (comment(X)%in%c("cwl", "swl")) {
                z <- lapply(qda.obj, function(x) CAPPER2(x, caps.list=caps.list))
                return(z)
            }
        }
    }
}
