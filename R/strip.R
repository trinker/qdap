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
strip <-
function(x, digit.remove = TRUE, apostrophe.remove = FALSE){
    strp <- function(x, digit.remove, apostrophe.remove){
        x2 <- Trim(tolower(gsub(".*?($|'|[^[:punct:]]).*?", 
            "\\1", as.character(x))))
        x2 <- if(apostrophe.remove) gsub("'", "", x2) else x2
        ifelse(digit.remove==TRUE, gsub("[[:digit:]]", "", x2), x2)
    }
    unlist(lapply(x, function(x) Trim(strp(x =x, 
        digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove)) ))
}
