termco.a <-
function (text.var, grouping.var=NULL, match.list, ignore.case = FALSE, 
    lazy.term = TRUE, elim.old = TRUE, zero.replace = 0, output = "percent", digits = 2) {
    IND <- unlist(lapply(match.list, length))
    new.names <- names(IND)[IND != 1]
    CC <- match.list[sapply(match.list, length) > 1]
    ML <- unlist(match.list) 
    TD <- termco.d(text.var = text.var, grouping.var = grouping.var, 
        match.string = ML, ignore.case = ignore.case, 
        zero.replace = zero.replace, output = output, digits = digits)
    if (sd(IND)==0){
        o <- TD
    } else {
        o <- termco.c(TD, combined.columns = CC, new.name = new.names, 
             zero.replace = NULL, lazy.term = lazy.term, elim.old = elim.old)
    }
return(o)
}
