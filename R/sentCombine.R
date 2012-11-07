sentCombine <-
function(text.var, grouping.var = "person") {
    y <- rle(as.character(grouping.var))
    lens <- y$lengths
    group <- y$values
    x <- cumsum(lens)
    st <- c(1, x[-length(x)]+1)
    end <- c(x)
    L1 <- lapply(seq_along(st), function(i) paste2(text.var[st[i]:end[i]], sep=" "))
    names(L1) <- group
    L1
}
