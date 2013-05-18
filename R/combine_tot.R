#Helper function used in read.transcript 
combine_tot <- 
  function(dataframe, combine.var = 1, text.var = 2) {
    NAMES <- colnames(dataframe)
    lens <- rle(as.character(dataframe[, combine.var]))
    z <- lens$lengths > 1
    z[lens$lengths > 1] <- 1:sum(lens$lengths > 1) 
    a <- rep(z, lens$lengths)
    dataframe[, "ID"] <- 1:nrow(dataframe)
    b <- split(dataframe, a)
    w <- b[names(b) != "0"]
    v <- lapply(w, function(x) {
      x <- data.frame(var1 = x[1, 1], 
                      text = paste(x[, text.var], collapse=" "),
                      ID = x[1, 3], stringsAsFactors = FALSE)
      colnames(x)[1:2] <- NAMES
      return(x)
    }
    )
    v$x <- as.data.frame(b["0"], stringsAsFactors = FALSE)
    colnames(v$x) <- unlist(strsplit(colnames(v$x), "\\."))[c(F, T)]
    h <- do.call(rbind, v)
    h <- h[order(h$ID), ][, -3]
    rownames(h) <- NULL
    return(h)
}
