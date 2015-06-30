#' Kullback Leibler Statistic
#' 
#' A proximity measure between two probability distributions applied to speech.
#' 
#' @param x A numeric vector, matrix or data frame.
#' @param y A second numeric vector if x is also a vector.  Default is 
#' \code{NULL}.
#' @return Returns a matrix of the Kullback Leibler measure between each vector 
#' of probabilities.
#' @details Uses Kullback & Leibler's (1951) formula:
#' \deqn{D_{KL}(P||Q)=\sum_i{ln\left ( \frac{P_{i}}{Q_{i}} \right )}P_{i}}
#' @note The \code{kullback_leibler} function generally receives the output of
#' either \code{wfm} or \code{wfdf} functions.
#' @references  Kullback, S., & Leibler, R.A. (1951). On Information and 
#' sufficiency. Annals of Mathematical Statistics 22 (1): 79-86. 
#' doi:10.1214/aoms/1177729694
#' @keywords Kullback-Leibler
#' @export
#' @importFrom qdapTools v_outer
#' @examples
#' \dontrun{
#' p.df <- wfdf(DATA$state, DATA$person)
#' p.mat <- wfm(text.var = DATA$state, grouping.var = DATA$person)
#' kullback_leibler(p.mat)
#' (x <- kullback_leibler(p.df))
#' print(x, digits = 5)
#' kullback_leibler(p.df$greg, p.df$sam)
#' 
#' ## p.df2 <- wfdf(raj$dialogue, raj$person)
#' ## x <- kullback_leibler(p.df2)
#' }
kullback_leibler <-
function(x, y = NULL){
    if(methods::is(x, "wfdf")){
        if (methods::is(x, "t.df")) {
            x <- x[, -c(1)]
        } else {
             if (methods::is(x, "m.df")) { 
                 x <- x[-nrow(x), -c(1, ncol(x))]
             } 
        }
    } else {
        x <- x
    }
    if (is.null(y)) { 
        z <- v_outer(x, kl)
    } else {
        z <- kl(x = x, y = y)
    }
    class(z) <- c("kullback_leibler", "data.frame")
    z
}

#' Prints a kullback_leibler Object.
#' 
#' Prints a kullback_leibler object.
#' 
#' @param x The kullback_leibler object
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print kullback_leibler
#' @export
print.kullback_leibler <-
function(x, digits = 3, ...) {
    if (length(x) == 1) {
        y <- unclass(x)
        print(y)
    } else {
        WD <- options()[["width"]]
        options(width=3000)
        class(x) <- "matrix"
        if (!is.null(digits)) {
            x <- round(x, digits = digits)
        }
        print(x)
        options(width=WD)  
    }
}

#' Plots a kullback_leibler object
#' 
#' Plots a kullback_leibler object.
#' 
#' @param x The kullback_leibler object
#' @param digits Number of decimal places to print. 
#' @param \ldots Other arguments passed to \code{qheat}
#' @method plot kullback_leibler
#' @export
plot.kullback_leibler <- function(x, digits = 3 , ...) {
    if (all(dim(x) == c(0, 1))) stop("Can not plot a single value")
    class(x) <- "matrix"
    diag(x) <- NA
    qheat(x, digits = digits, ...)
}


## Helper function:
kl <- function(x, y){
    x1 <- x/sum(x)
    y1 <- y/sum(y)
    x1[x1==0] <- NA
    y1[y1==0] <- NA
    z <- stats::na.omit(data.frame(x1, y1))
    sum(z[, 1] * log(z[, 1]/z[, 2]))
}
