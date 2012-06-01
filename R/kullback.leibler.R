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
