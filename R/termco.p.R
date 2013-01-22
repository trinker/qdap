#helper function for termco_d and termco (not exported) 
termco.p <-
function(tco, percent = TRUE, short.term = FALSE){
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    a <- subdf(tco, -c(1:2))
    b <- tco[, 2]
    e <- tco[, 1:2]
    if (percent) { 
        d <- lapply(a, function(x) 100*(x/b))
    } else {
        d <- lapply(a, function(x) x/b)
    }
    o <- data.frame(e, d, check.names = FALSE)
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}