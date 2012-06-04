rm.gantt <-
function(rm.var, text.var, grouping.var, units = "words"){
    g <- grouping.var
    r <- rm.var
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        m <- gsub(".", "", m, fixed = TRUE)
        paste(m, collapse=".")
    } else {
        G1 <- as.character(substitute(grouping.var))
        G1[length(G1)]
    }
    NAME2 <- if (is.list(rm.var)) {
        m2 <- unlist(as.character(substitute(rm.var))[-1])
        m2 <- sapply(strsplit(m2, "$", fixed=TRUE), 
            function(x) x[length(x)])
        m2 <- gsub(".", "", m2, fixed = TRUE)
        paste(m2, collapse=".")
    } else {
        G2 <- as.character(substitute(rm.var))
        G2[length(G2)]
    }
    rm.var <- if (is.list(rm.var) & length(rm.var)>1) {
        apply(data.frame(rm.var), 1, function(x){
                if (any(is.na(x))) {
                    NA 
                } else {
                    paste(x, collapse = ".")
                }
            }
        )
    } else {
        rm.var
    }  
    grouping.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
        apply(data.frame(grouping.var), 1, function(x){
            if (any(is.na(x)))NA else paste(x, collapse = ".")
            }
        )
    } else {
        grouping.var
    } 
    DAT <- data.frame(rm.var, grouping.var, text.var)
    DAT2 <- split(DAT, rm.var)

    DAT3 <- lapply(seq_along(DAT2), function(i) {
            rm1 <- DAT2[[i]][, 1]
            gn <- DAT2[[i]][, -1]
            gn2 <- gantt.plot(gn[, "text.var"], gn[, "grouping.var"], 
                plot = FALSE, units = units)
            gn3 <- data.frame(rm.var = rm1[nrow(gn2)], gn2)
            return(gn3)
        }
    )
    DAT3 <- do.call("rbind", DAT3)
    names(DAT3)[1:2] <- c(NAME2, NAME)
    row.names(DAT3) <- 1:nrow(DAT3)
    nrf2 <- sum(gregexpr("[.]", names(DAT3[, 1, drop = FALSE]))[[1]] < 0)
        if (nrf2==0) RMV <- colSplit(DAT3[, 1, drop = FALSE], name.sep = ".")
    nrf <- sum(gregexpr("[.]", names(DAT3[, 2, drop = FALSE]))[[1]] < 0)
        if (nrf==0) GV <- colSplit(DAT3[, 2, drop = FALSE], name.sep = ".")
    DAT4 <- if (nrf==0){
        data.frame(DAT3[, 2, drop =FALSE], GV, DAT3[, -c(1:2)])
    } else {
        data.frame(DAT3[, 2, drop =FALSE], DAT3[, -c(1:2)])
    }
    DAT3 <- if (nrf2==0){
        data.frame(DAT3[, 1, drop =FALSE], RMV, DAT4)
    } else {
        data.frame(DAT3[, 1, drop =FALSE], DAT4)
    }
    return(DAT3)
}
