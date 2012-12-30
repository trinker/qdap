#' Kullback Leibler Statistic
#' 
#' A proximatey measure between two probability distributions applied to speech.
#' 
#' @param x A numeric vector, matrix or data frame.
#' @param y A second numeric vector if x is also a vector.  Default is NULL.
#' @param digits Number of decimal places to round. 
#' @return Returns a matrix of the Kullback Leibler measure between each vector 
#' of probabiltiies.
#' @details Uses Kullback & Leibler's (1951) formula:
#' \deqn{D_{KL}(P||Q)=\sum_i{ln\left ( \frac{P_{i}}{Q_{i}} \right )}P_{i}}
#' @references  Kullback, S., & Leibler, R.A. (1951). On Information and 
#' sufficiency. Annals of Mathematical Statistics 22 (1): 79-86. 
#' doi:10.1214/aoms/1177729694
#' @keywords Kullback-Leibler
#' @examples
#' \dontrun{
#' p.df <- word.freq.df(DATA$state, DATA$person)                    
#' p.mat <- wfm(text.var = DATA$state, grouping.var = DATA$person)  
#'                                                                 
#' kullback.leibler(p.mat)                                         
#' kullback.leibler(p.df)                                          
#' kullback.leibler(p.df$greg, p.df$sam)  
#' 
#' p.df2 <- word.freq.df(raj$dialogue, raj$person)
#' kullback.leibler(p.df2)
#' }
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
        z <- v.outer(x, "kl", digits = digits)
    } else {
        z <- round(kl(x = x, y = y), digits = digits)
    }
    z
}