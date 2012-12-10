#' Kullback Leibler Statistic
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param y %% ~~Describe \code{y} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
kullback.leibler <-
function(x, y = NULL, digits = 3){
    kl <- function(x, y){
        x1 <- x/sum(x)
        y1 <- y/sum(y)
        x1[x1==0] <- NA
        y1[y1==0] <- NA
        z <- na.omit(data.frame(x1, y1))
        sum(z[, 1] * log(z[, 1]/z[, 2]))
    }
    if(is.null(y) & !is.null(comment(x))){
        if (comment(x) %in% c("t.df")) {
            x <- x[, -c(1)]
        } else {
             if (is.null(y) & comment(x) %in% c("m.df")) { 
                 x <- x[-nrow(x), -c(1, ncol(x))]
             } else { 
                 x <- x
             }
        }
    } else {
        x <- x
    }

    if (is.null(y)) { 
        z <- outer (
            colnames(x), 
            colnames(x), 
            Vectorize(function(i,j) kl(x[,i],x[,j]))
        )
        dimnames(z) <- list(colnames(x), colnames(x))
    } else {
        z <- kl(x = x, y = y)
    }
    z <- round(z, digits = digits)
    return(z)
}
