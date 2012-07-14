formality <-
function(text.var, grouping.var = NULL, sort.by.formality = TRUE){
    x <- "\n\nHeylighen, F., & Dewaele, J.-M. (2002). Variation in the contextuality of language: an
        empirical measure. Context in Context, Special issue of Foundations of Science, 7 (3),
        293–340.\n\n\n\n\n"
    G <- if(is.null(grouping.var)) {
             "all"
         } else {
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) x[length(x)])
                     paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    X <- pos.by(text.var = text.var, grouping.var = grouping.var)
    nameX <- rownames(X)
    X <- data.frame(X)
    xn <- nrow(X)
    X$JI <- rep(0, xn)
    X$JK <- rep(0, xn)
    DF1 <- data.frame( 
        noun = rowSums(X[, names(X) %in% c('NN', 'NNS', 'NNP', 'NNPS', 'POS', 'JI', 'JK')]), 
        verb = rowSums(X[, names(X) %in% c('MD', 'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ', 'JI', 'JK')]),  
        adverb = rowSums(X[, names(X) %in% c('RB', 'RBR', 'RBS', 'WRB', 'JI', 'JK')]),  
        pronoun = rowSums(X[, names(X) %in% c('PRP', 'PRP.', 'WDT', 'WP', 'WP.', 'JI', 'JK')]),  
        prep = rowSums(X[, names(X) %in% c('IN', 'RP', 'TO', 'JI', 'JK')]),  
        adj = rowSums(X[, names(X) %in% c('CD', 'DT', 'JJ', 'JJR', 'JJS', 'JI', 'JK')]),  
        interj = rowSums(X[, names(X) %in% c('UH', 'JI', 'JK')]))

    FOR <- (rowSums(cbind(DF1$noun, DF1$adj, DF1$prep)) - rowSums(cbind(DF1$pronoun, 
        DF1$verb, DF1$adverb, DF1$interj)) + 100)/2
    if(!is.null(grouping.var)) {
        FOR <- data.frame(replace = X[, 1], formality = FOR); colnames(FOR)[1] <- G
    }
    if (sort.by.formality) {
        FOR <- FOR[order(-FOR$formality), ]
        rownames(FOR) <- NULL
    }
    return(FOR)
}
