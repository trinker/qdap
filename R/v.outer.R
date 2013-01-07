#' Vectorized Version of outer
#' 
#' Vectorized \code{\link[base]{outer}}. 
#' 
#' @param x A \code{matrix}, \code{dataframe} or equal length \code{list} of 
#' vectors.
#' @param FUN A vectorized function.
#' @param digits Integer; number of decimal places to round. 
#' @param \ldots Other arguments passed to the function supplied to \code{FUN}.
#' @return Returns a matrix with the vectorized \code{\link[base]{outer}} 
#' function.
#' @seealso \code{\link[base]{outer}},
#' \code{\link[stats]{cor}}
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
#' v.outer(mtcars, euc.dist)
#' v.outer(mtcars, sum2)
#' 
#' 
#' mtcars2 <- lapply(mtcars, function(x) x)
#' v.outer(mtcars2, cor)
#' v.outer(mtcars2, cor,  method = "spearman")
#' v.outer(mtcars2, pooled.sd)
#' v.outer(mtcars2, euc.dist)
#' v.outer(mtcars2, sum2)
#' 
#' wc3 <- function(x, y) sum(sapply(list(x, y), wc, byrow = FALSE))
#' L1 <- word_list(DATA$state, DATA$person)$cwl
#' v.outer(L1, wc3)
#' }
v.outer <- 
function(x, FUN, digits = 3, ...){
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
    if (is.numeric(z)) {
        z <- round(z, digits = digits)
    }
    z
}