#helper function for termco_d and termco (not exported) 
termco.p <-
function(tco, percent = TRUE, short.term = FALSE){
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    a <- subdf(tco, -c(1:2))
    b <- tco[, 2]
    e <- tco[, 1:2]
    
    ## Added nan2zero on 1-21-14 to deal with infinite values caused by NA
    if (percent) { 
        d <- lapply(a, function(x) nan2zero(100*(x/b)))
    } else {
        d <- lapply(a, function(x) nan2zero(x/b))
    }
    o <- data.frame(e, d, check.names = FALSE)
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}

## Helper function added on 1-21-14
## Used in termco_c as well
nan2zero <- function(x) {
    x[is.nan(x)] <- 0
    x
}

