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
symbol_change <-
function(text.var, dollar = TRUE, percent = TRUE, 
    pound = TRUE, at = TRUE, and = TRUE) {
    
    sch <- function(text, dollar = TRUE, percent = TRUE, pound = TRUE, 
        at = TRUE, and = TRUE) {
            x <- ifelse(percent == TRUE, gsub("%", " percent ", 
                text), text)
            x <- ifelse(dollar == TRUE, gsub("$", " dollars ", x, 
                fixed = TRUE), x)
            x <- ifelse(pound == TRUE, gsub("#", " number ", x, 
                fixed = TRUE), x)
            x <- ifelse(and == TRUE, gsub("&", " and ", x, fixed = TRUE), x)
            x <- ifelse(at == TRUE, gsub("@", " at ", x, fixed = TRUE), x)
            gsub(" +", " ", x)
    }
    unlist(lapply(text.var, function(text) sch(text, dollar = dollar, 
        percent = percent, pound = pound, at = at, and = and)))
}
