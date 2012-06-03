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
character.count <-
function(text, by = "row", missing = NA, 
    apostrophe = TRUE, digit.remove = TRUE) {
    text2 <- if (apostrophe == TRUE) {
        text
    } else {
        gsub("'", "", text, fixed = TRUE)
    }
    chara <- function(x) {
        y <- unlist(strsplit(strip(x, digit.remove = digit.remove), 
            NULL))
        z <- subset(y, y != " ")
        length(z)
    }
    OP <- switch(by, 
           all = chara(paste(unlist(text2), collapse = "  ")), 
           row = unlist(lapply(text2, function(x) chara(x)))
          )
    ifelse(OP == 0, missing, OP)
}
