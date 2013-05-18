#' Vectorized Version of outer
#' 
#' Vectorized \code{\link[base]{outer}}. 
#' 
#' @param x A \code{matrix}, \code{dataframe} or equal length \code{list} of 
#' vectors.
#' @param FUN A vectorized function.
#' @param \ldots Other arguments passed to the function supplied to \code{FUN}.
#' @return Returns a matrix with the vectorized \code{\link[base]{outer}} 
#' function.
#' @seealso \code{\link[base]{outer}},
#' \code{\link[stats]{cor}}
#' @author Vincent Zoonekynd and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @export
#' @examples
#' \dontrun{
#' pooled.sd <- function(x, y) {
#'     n1 <- length(x)
#'     n2 <- length(y)
#'     s1 <- sd(x)
#'     s2 <- sd(y)
#'     sqrt(((n1-1)*s1 + (n2-1)*s2)/((n1-1) + (n2-1)))
#' }
#' 
#' euc.dist <- function(x,y) sqrt(sum((x - y) ^ 2))
#' sum2 <- function(x, y) sum(x, y)
#' 
#' v.outer(mtcars, cor)
#' v.outer(mtcars, pooled.sd)
#' v.outer(mtcars[, 1:7], euc.dist)
#' v.outer(mtcars[, 1:7], sum2)
#' 
#' #mtcars as a list
#' mtcars2 <- lapply(mtcars[, 1:7], "[")
#' v.outer(mtcars2, cor)
#' v.outer(mtcars2, cor,  method = "spearman")
#' v.outer(mtcars2, pooled.sd)
#' print(v.outer(mtcars[, 1:7], pooled.sd), digits = 1)
#' print(v.outer(mtcars[, 1:7], pooled.sd), digits = NULL)
#' v.outer(mtcars2, euc.dist)
#' v.outer(mtcars2, sum2)
#' 
#' wc3 <- function(x, y) sum(sapply(list(x, y), wc, byrow = FALSE))
#' L1 <- word_list(DATA$state, DATA$person)$cwl
#' (x <- v.outer(L1, wc3))
#' diag(x) <- (sapply(L1, length))
#' x
#' }
v.outer <- 
function(x, FUN, ...){
    FUN <- match.fun(FUN)
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
    if (is.list(x) & !is.data.frame(x)){
        if (is.null(names(x))) {
            names(x) <- paste0("X", seq_along(x))
        }
        nms <- names(x)   
    } else {
        nms <- colnames(x)
    }
    z <- outer(
      nms, 
      nms, 
      Vectorize(function(i,j) FUN(unlist(x[[i]]), unlist(x[[j]]), ...))
    )
    dimnames(z) <- list(nms, nms)
    class(z) <- "v.outer"
    z
}

#' Prints a v.outer Object.
#' 
#' Prints a v.outer object.
#' 
#' @param x The v.outer object
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print v.outer
#' @S3method print v.outer
print.v.outer <-
function(x, digits = 3, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    y <- unclass(x)
    if (is.numeric(y) & !is.null(digits)) {
        y <- round(y, digits = digits)
    }    
    print(y)
    options(width=WD)  
}
