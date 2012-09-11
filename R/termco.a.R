termco.a <-
function (text.var, grouping.var=NULL, match.list, short.term = FALSE,
    ignore.case = TRUE, lazy.term = TRUE, elim.old = TRUE, zero.replace = 0, 
    output = "percent", digits = 2) {
    preIND <- match.list
    IND <- unlist(lapply(preIND, length))
    new.names <- paste0("term(", names(IND)[IND != 1], ")")
    CC <- match.list[sapply(match.list, length) > 1]
    ML <- unlist(match.list) 
    TD <- termco.d(text.var = text.var, grouping.var = grouping.var, 
        match.string = ML, ignore.case = ignore.case, 
        zero.replace = zero.replace, output = output, digits = digits)
    if (is.list(preIND)) {
        if(length(IND) == sum(IND)){
            o <- TD
        } else {
            o <- termco.c(TD, combined.columns = CC, new.name = new.names, 
                zero.replace = NULL, lazy.term = lazy.term, elim.old = elim.old)
        }
    } else {
        o <- TD
    }
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}