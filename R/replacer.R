replacer <-
function(dat, replace=0, with="-"){ 
    h <- is.vector(dat)
    i <- is.matrix(dat)
    j <- is.data.frame(dat)
    if (is.numeric(replace)){
          NAMES <- names(dat)
          not.num <- Negate(is.numeric)
          dat1 <- dat[, sapply(dat, not.num)]
          dat2 <- dat[, sapply(dat, is.numeric)]
    } else {
          dat2 <- dat
    }
    y <- as.matrix(dat2)
    if (is.na(replace)) {
          y[is.na(y)] <- with
    } else { 
          y[y==replace] <- with
    }
    if (is.numeric(replace)){    
        y <- data.frame(dat1, y, check.names = FALSE)
        m <- NAMES[!NAMES%in%names(y)]
        if(length(m) == 1) names(y)[1] <- m
        y <- y[, NAMES]
    }
    if(h) y <- as.vector(y)
    if(i) y <- as.matrix(y)
    if(j) y <- as.data.frame(y, check.names = FALSE)
    return(y)
}
