termco.p <-
function(tco, output = "percent", digits = 2){
    a <- tco[, -c(1:2)]
    b <- tco[, 2]
    e <- tco[, 1:2]
    d <- switch(output, 
           percent = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           proportion = lapply(a, function(x) round(x/b, digits=digits)),
           per = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           prop = lapply(a, function(x) round(x/b, digits=digits))
         )
    return(data.frame(e, d, check.names = FALSE))
}
