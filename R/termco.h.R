#helper function for termco_d and termco (not exported) 
termco.h <-
function (text.var, match.string, grouping.var = NULL, ignore.case = FALSE, 
          zero.replace = 0){
  group.var <- grouping.var
  if (ignore.case) {
    x <- lapply(text.var, function(x) term.count(tolower(x), 
      mat = tolower(match.string)))
  } else {
    x <- lapply(text.var, function(x) term.count(x, mat = match.string))
  }
  group.var <- if(is.null(group.var)){
    rep("all", length(text.var))
  } else {
    if (is.list(group.var) & length(group.var) > 1) {
      apply(data.frame(group.var), 1, function(x) {
        if (any(is.na(x))) {
          NA
        } else {
          paste(x, collapse = ".")
        }
      })
    } else {
      grouping.var
    }
  }
  if (lapply(x, length)[[1]] == 1){
    y <- data.frame(text.var, do.call("c", x))
  } else {
    y <- data.frame(text.var, do.call("rbind", x))
  }

  if (is.null(group.var)) {
    names(y) <- c(as.character(substitute(text.var))[3], 
      paste("term(", match.string, ")", sep = ""))
    return(y)
  } else {
    y <- data.frame(group.var, y[, -1])
    X <- data.frame(Y = word_count(text.var), G = group.var)

    ## Added on 1-21-14 to deal with NA in data
    X[is.na(X[, "Y"]), "Y"] <- 0

    Z <- stats::aggregate(Y ~ G, X, sum)
    z <- lapply(2:length(y), function(x) {
      stats::aggregate(y[, x] ~ group.var, y, sum)
    })

    w <- data.frame(z[[1]][1], Z[, 2], lapply(seq_along(z), 
      function(x) z[[x]][, 2]))
    NAME <- if (is.null(grouping.var)) {
      "all"
    } else {
      if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
        paste(m, collapse = "&")
      } else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
      }
    }
    names(w) <- c(NAME, "word.count", paste("term(", match.string, 
      ")", sep = ""))
    w[, -c(1:2)] <- replacer(w[, -c(1:2), drop=FALSE], replace = 0, 
      with = zero.replace)
    attributes(w)[["by.row"]] <- stats::setNames(data.frame(
        X[, 2:1], 
        y[, -1, drop=FALSE]
    ), names(w))
    return(w)
  }
}
