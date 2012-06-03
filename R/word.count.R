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
word.count <- 
function(text, by = "row", missing = NA, 
    digit.remove = TRUE, names = TRUE) {
    txt <- sapply(as.character(text), function(x) {
            ifelse(is.na(x)|is.null(x), "", x)
        }
    )
    OP <- switch(by, 
         all = {length(unblanker(unlist(word.split(reducer(unlist(strip(
                   paste(as.character(txt), collapse = " "), 
                   digit.remove = digit.remove)))))))}, 
         row = {sapply(txt, function(x) length(unblanker(unlist(word.split(
                   reducer(unlist(strip(as.character(x), digit.remove = 
         digit.remove))))))))}
    )
    z <- ifelse(OP==0, missing, OP)
    if(!names) names(z) <- NULL
    return(z)
}

wc <- word.count