#' SPSS Style Frequency Tables
#' 
#' Generates a dsitribution table for vectors, matrices and data.frames
#' 
#' @aliases distTab
#' @param dataframe a vector or data.frame object
#' @param breaks either a numeric vector of two or more cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut 
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @param \lodots other variables passed to cut
#' @return a list of data frames (or singular data frame for a vector) of frequencies, cumulative frequencies, percentages and cumalative percentages for each interval
#' @seealso \code{\link[base]{cut}}
#' @keywords distribution 
#' @examples
#' distTab(rnorm(10000), 10)
#' distTab(sample(c("red", "blue", "gray"), 100, T), right = FALSE)
#' distTab(CO2, 4)
#' distTab(mtcars)
#' distTab(mtcars, 4)
distTab <-
function(dataframe, breaks = NULL, digits = 2, ...){
    DF <- as.data.frame(dataframe)
    freq.dist <- function (var, breaks, digits, ...){
        x1 <- substitute(var)
        if (is.null(breaks)){
            VAR <- var
        } else {
            if (is.numeric(var) & length(table(var)) > breaks){
                VAR <- cut(var, breaks, ...)
            } else {
                VAR <- var
            }
        }
        x <- data.frame(table(VAR))
        names(x)[1] <- as.character(x1)
        percent <- x[, 2]/sum(x[, 2])*100
        data.frame(interval = x[, 1], Freq = x[, 2],
            cum.Freq = cumsum(x[, 2]), 
            percent = round(percent, digits = digits),
            cum.percent = round(cumsum(percent), digits = digits))
    }
    z <- lapply(DF, function(x) freq.dist(x, breaks = breaks, digits = digits, ...))
    if (is.vector(dataframe)) {
        z <- z[[1]]
    }
    z
}
