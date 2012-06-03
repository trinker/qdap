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
bracketX <-
function(text, bracket='all', missing=NULL){
    X <- switch(bracket,
        square=sapply(text, function(x)gsub("\\[.+?\\]", "", x)),
        round=sapply(text, function(x)gsub("\\(.+?\\)", "", x)),
        curly=sapply(text, function(x)gsub("\\{.+?\\}", "", x)),
        all={P1<-sapply(text, function(x)gsub("\\[.+?\\]", "", x))
             P1<-sapply(P1, function(x)gsub("\\(.+?\\)", "", x))
             sapply(P1, function(x)gsub("\\{.+?\\}", "", x))
             }
    ) 
    X <- Trim(gsub(" +", " ", X))
    if (!is.null(missing)) {X[X==""] <- missing}
    return(X)                                                                                                                                                         
}
