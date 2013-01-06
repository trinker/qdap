#helper function for termco_d and termco_a (not exported) 
termco.p <-
function(tco, output = "percent", short.term = FALSE, digits = 2){
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    a <- subdf(tco, -c(1:2))
    b <- tco[, 2]
    e <- tco[, 1:2]
    d <- switch(output, 
           percent = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           proportion = lapply(a, function(x) round(x/b, digits=digits)),
           per = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           prop = lapply(a, function(x) round(x/b, digits=digits))
         )
    o <- data.frame(e, d, check.names = FALSE)
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}
