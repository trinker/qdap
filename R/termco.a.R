termco.a <-
function (text.var, grouping.var=NULL, match.list, short.term = TRUE,
    ignore.case = TRUE, lazy.term = TRUE, elim.old = TRUE, zero.replace = 0, 
    output = "percent", digits = 2, ...) {
    mprot <- names(match.list) != "" & sapply(match.list, length) == 1
    NAME <- if (is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), 
                function(x) x[length(x)])
            paste(m, collapse = "&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    preIND <- match.list
    IND <- unlist(lapply(preIND, length))
    new.names <- paste0("term(", names(IND)[IND != 1], ")")
    CC <- match.list[sapply(match.list, length) > 1]
    ML <- unlist(match.list) 
    TD <- termco.d(text.var = text.var, grouping.var = grouping.var, 
        match.string = ML, ignore.case = ignore.case, zero.replace = zero.replace, 
        output = output, digits = digits, ...)
    if (is.list(preIND)) {
        if(length(IND) == sum(IND)){
            o <- TD
        } else {
            o <- termco.c(TD, combined.columns = CC, new.name = new.names, 
                zero.replace = NULL, lazy.term = lazy.term, elim.old = elim.old,
                output = output)
            if (elim.old) {
                names(match.list)[names(match.list) == ""] <- unlist(match.list[names(match.list) == ""])
                tailend <- paste0("term(", names(match.list)[names(match.list) != ""], ")")
                subdf <- function(df, ii) {
                  do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
                }
                INDS <- lapply(tailend, function(x) {
                    x
                    inds <- which(colnames(o[["raw"]]) == x)
                    if(identical(inds, integer(0))){
                        inds <- which(names(match.list) == bracketXtract(x))-1
                    }
                    inds
                })
                keeps <- c(1:2, sapply(INDS, max))
                lapply(1:3, function(i) {
                        o[[i]] <<- subdf(o[[i]], keeps)
                        pv <- match.list[mprot]
                        pv2 <- colnames(o[[i]]) %in% paste0("term(", pv, ")")
                        colnames(o[[i]])[pv2] <<- names(match.list)[pv2[-c(1:2)]]
                    }
                )
            }
        }
    } else {
        o <- TD
    }
    o[1:3] <- lapply(o[1:3], function(x) {
        colnames(x)[1] <- NAME
        rownames(x) <- NULL
        return(x)
    })
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}