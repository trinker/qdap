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
distTab <-
function(dataframe, cuts=NULL){
    DF <- as.data.frame(dataframe)
    freq.dist <- function (var, breaks=cuts){
        x1 <- substitute(var)
        VAR <- if (is.null(cuts)){
                   var
               } else {
                   if (is.numeric(var)&length(table(var))>cuts){
                       cut(var, breaks, labels = NULL,  
                           include.lowest = FALSE, right = TRUE, 
                           dig.lab = 3, ordered_result = FALSE)
                   } else {
                       var
                   }
               }
        x <- data.frame(table(VAR))
        names(x)[1] <- as.character(x1)
        percent <- x[, 2]/sum(x[, 2])*100
            data.frame("interval"=x[, 1], "Freq"=x[, 2],
            "cum.Freq"=cumsum(x[, 2]), percent, 
            "cum.percent"=cumsum(percent))
        }
    suppressWarnings(lapply(DF, FUN=freq.dist))
}
