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
term.count <-
function(str, mat){
    tester <- function(x, y){
        p <- suppressWarnings(unlist(gregexpr(x, y, fixed = FALSE)))
        j <- suppressWarnings(if(is.na(str) | length(p) == 1 & p<1) { 
                0 
            } else {
                length(p)
            }
        )
        return(j)
    }
    spacer <- function(string){
        sapply(string, function(x) paste0(" ", x, " "), USE.NAMES = FALSE)
    }
    str <- spacer(str)
    y <- sapply(mat, function(x) tester(x, str))
    return(y)
}
