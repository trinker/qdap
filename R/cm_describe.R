#helper function used in cm_distance(not for export) 
cm_describe <-
function(code, grouping.var = NULL) {
    if (!is.null(grouping.var)) {
        if (is.list(grouping.var)) {
          m <- unlist(as.character(substitute(grouping.var))[-1])
          m <- sapply(strsplit(m, "$", fixed = TRUE), 
                      function(x) x[length(x)])
          NAME <- paste(m, collapse = "&")
        } else {
          G <- as.character(substitute(grouping.var))
          NAME <- G[length(G)]
        }
        cname <- strsplit(as.character(substitute(code)), "&")
        NAME <- paste0(cname[[length(cname)]], "&", NAME)
        group.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
          apply(data.frame(grouping.var), 1, function(x){
            if (any(is.na(x))){
              NA
            } else {
              paste(x, collapse = ".")
            }
          }
          )
        } else {
          grouping.var
        }
        v <- do.call(data.frame, rle(paste2(list(code, group.var))))
    } else {
        v <- do.call(data.frame, rle(code))
    }
    v$end<- cumsum(v[, 1])
    colnames(v)[1] <- "duration"
    v$start <- c(0, c(v$end)[-c(length(v$end))])
    v$center <- (v$start + v$end)/2
    v2 <- v[, c("values", "center", "duration", "start", "end")]
    if (!is.null(grouping.var)) {
        nl <- if (is.list(grouping.var)) {
            grouping.var 
        } else { 
            list(grouping.var)
        }
        L2 <- lapply(1:(length(nl) + 1), function(i) {
            x <- strsplit(as.character(v2[, "values"]), "\\.")
            sapply(1:length(x), function(j)x[[j]][i])
            }
        )
        v3 <- data.frame(do.call(cbind, L2))
        colnames(v3) <- unlist(strsplit(NAME, "\\&"))
        v2 <- data.frame(v3, v2[, -1, drop=FALSE])      
    } else {
        cname <- strsplit(as.character(substitute(code)), "&")
        colnames(v2)[1] <- cname[[length(cname)]]
    }
    return(v2)
}
