formality <- function(text.var, grouping.var = NULL, sort.by.formality = TRUE,
    digits = 2){
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
    if (!class(text.var) %in% c("POS", "POSby", "formality.measure")) {
        WOR <- word.count(text.var)
    } else {
        WOR <- word.count(text.var$text)
    }
    pos.list <- pos.by(text.var = text.var, grouping.var = grouping.var, 
        digits = digits)
    X <- pos.list[["pos.by.freq"]]
    nameX <- rownames(X)
    X <- data.frame(X)
    xn <- nrow(X)
    X$JI <- rep(0, xn)
    X$JK <- rep(0, xn)
    article <- function(x) {
        WORDS <- stopwords(x, stopwords = NULL, 
            unlist = FALSE, strip = TRUE)
        sapply(WORDS, function(x) sum(x %in% c("the", "an", "a")))
    }
    if (!is.null(grouping.var)){
        articles <- unlist(lapply(split(text.var, grouping.var), function(x) 
            sum(article(x))))
    } else {
        articles <- sum(article(text.var))
    }
    if (!is.null(X$DT)) {
        PD <- X$DT-articles
    }
    DF1 <- data.frame( 
        noun = rowSums(X[, names(X) %in% c("NN", "NNS", "NNP", "NNPS", 
            "POS", "JI", "JK")]), 
        articles = articles,
        verb = rowSums(X[, names(X) %in% c("MD", "VB", "VBD", "VBG", 
            "VBN", "VBP", "VBZ", "JI", "JK")]),  
        adverb = rowSums(X[, names(X) %in% c("RB", "RBR", "RBS", "WRB", 
            "JI", "JK")]),  
        pronoun = rowSums(X[, names(X) %in% c("PRP", "PRP$", "PRP.", "WDT", 
            "WP", "WP$", "WP.", "JI", "JK", "EX")]),  
        prep = rowSums(X[, names(X) %in% c("IN", "RP", "TO", "JI", "JK")]),  
        adj = rowSums(cbind(X[, names(X) %in% c("CD", "JJ", "JJR", "JJS", 
            "JI", "JK")], PD)),  
        interj = rowSums(X[, names(X) %in% c("UH", "JI", "JK")]))
    DF1RS <- rowSums(DF1)
    DF1 <- do.call(rbind, lapply(1:nrow(DF1), function(i) 100*(DF1[i, ]/DF1RS[i])))
    FOR <- (rowSums(cbind(DF1$noun, DF1$article, DF1$adj, DF1$prep)) - 
        rowSums(cbind(DF1$pronoun, DF1$verb, DF1$adverb, DF1$interj)) + 100)/2
    WOR <- sapply(split(WOR, grouping.var), sum, na.rm = TRUE)
    if(!is.null(grouping.var)) {
        FOR <- data.frame(replace = X[, 1], word.count = WOR, formality = FOR)
        colnames(FOR)[1] <- G
    }
    FOR[, "formality"] <- round(FOR[, "formality"], digits = digits)
    if (!is.null(grouping.var) & sort.by.formality) {
        FOR <- FOR[order(-FOR$formality), ]
        rownames(FOR) <- NULL
    }
    o <- unclass(pos.list)
    o$formality <- FOR
    class(o) <- "formality.measure"
    return(o)
}