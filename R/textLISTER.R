#helper function for word_list (not exported)
textLISTER <-
function(text.var, group.vars, rm.bracket = TRUE, char.keep = NULL,  
    apostrophe.remove = FALSE, ...) {
    NAME <- if (is.list(group.vars)) {
        m <- unlist(as.character(substitute(group.vars))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }, USE.NAMES = FALSE
        )
        paste(m, collapse="&")
    } else {
        G <- as.character(substitute(group.vars))
        G[length(G)]
    }
    DF <- data.frame(text.var, group.vars)
    if (NAME == "") {
      NAME <- "all"
      names(DF)[2] <- "group.vars"
    }
    if (rm.bracket) {
        DF$dia2word <- Trim(as.character(bracketX(DF[, 1])))
    } else {
        DF$dia2word <- Trim(as.character(DF[, 1]))   
    }
    if (nrow(DF) == 1) {
      DF <- do.call("rbind", list(DF, DF))
      DF[2, 3] <- "void"
      DF$dia2word <- as.vector(word_split(reducer(strip(DF$dia2word, 
          char.keep = char.keep, apostrophe.remove = apostrophe.remove, ...))))
      DF <- DF[1, ]
    } else {
      DF$dia2word <- as.vector(word_split(reducer(strip(DF$dia2word, 
          char.keep = char.keep, apostrophe.remove = apostrophe.remove, ...))))
    }     
    X <- split(DF[, -1], DF$group.vars)
    NAMES <- names(X)
    X <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 2])
    X <- lapply(X, function(x) {
            v <- unlist(x)
            names(v) <- NULL
            return(v)
        }
    )
    X <- lapply(X, function(x) {
            data.frame(words=x, stringsAsFactors = FALSE)
        }
    )
    names(X) <- NAMES
    X
}
