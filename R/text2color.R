text2color <-
function(words, recode.words, colors) {
    nc <- length(colors)
    nomatch <- colors[nc]
    colors <- colors[-nc]
    lookup <- lapply(seq_along(recode.words), function(n) 
        cbind(recode.words[[n]], colors[n]))
    lookup <- do.call("rbind.data.frame", lookup)
    lookup <- apply(lookup, 2, as.character)
    recode <- lookup[match(words, lookup[, 1]), 2]
       recode[is.na(recode)] <- nomatch
    return(recode)
}
