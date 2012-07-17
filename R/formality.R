formality <- function(text.var, grouping.var = NULL, plot = FALSE,
    sort.by.formality = TRUE, digits = 2){
    G <- if(is.null(grouping.var)) {
             gv <- TRUE
             "all"
         } else {
             gv <- FALSE
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) x[length(x)])
                     paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    grouping.var <- if(is.null(grouping.var)){
        rep("all", length(text.var))
    } else {
    if(is.list(grouping.var) & length(grouping.var)>1) {
         apply(data.frame(grouping.var), 1, function(x){
                     if (any(is.na(x))){
                         NA
                     }else{
                         paste(x, collapse = ".")
                     }
                 }
             )
        } else {
            unlist(grouping.var)
        } 
    }
    if (!gv) {
        pos.list <- pos.by(text.var = text.var, 
            grouping.var = grouping.var, digits = digits)
    } else {
        pos.list <- suppressWarnings(pos.by(text.var = text.var, 
            grouping.var = NULL, digits = digits))
    }
    text.var <- pos.list$text
    WOR <- word.count(text.var)
    X <- pos.list[["pos.by.freq"]]
    nameX <- rownames(X)
    X <- data.frame(X)
    xn <- nrow(X)
    X$JI <- rep(0, xn)
    X$JK <- rep(0, xn)
    article <- function(x) {
        if (identical(x, character(0))) {
            return(0)
        } else {
            WORDS <- stopwords(x, stopwords = NULL, 
                unlist = FALSE, strip = TRUE)
            sapply(WORDS, function(x) sum(x %in% c("the", "an", "a"),
                na.rm = TRUE ))
        }
    }
    if (!is.null(grouping.var)){
        stv <- split(text.var, grouping.var)
        stv <- stv[sapply(stv, function(x) !identical(x, character(0)))]
        articles <- unlist(lapply(stv, function(x){ 
                sum(article(x))
            }
        )) 
    } else {
        articles <- sum(article(text.var$text))
    }
    if (!is.null(X$DT)) {
        PD <- X$DT-articles
    }
    DF1 <- DF2 <- data.frame( 
        noun = rowSums(X[, names(X) %in% c("NN", "NNS", "NNP", "NNPS", 
            "POS", "JI", "JK")]),
        adj = rowSums(cbind(X[, names(X) %in% c("CD", "JJ", "JJR", "JJS", 
            "JI", "JK")], PD)),   
        prep = rowSums(X[, names(X) %in% c("IN", "RP", "TO", "JI", "JK")]),  
        articles = articles,
        pronoun = rowSums(X[, names(X) %in% c("PRP", "PRP$", "PRP.", "WDT", 
            "WP", "WP$", "WP.", "JI", "JK", "EX")]),  
        verb = rowSums(X[, names(X) %in% c("MD", "VB", "VBD", "VBG", 
            "VBN", "VBP", "VBZ", "JI", "JK")]),  
        adverb = rowSums(X[, names(X) %in% c("RB", "RBR", "RBS", "WRB", 
            "JI", "JK")]),  
        interj = rowSums(X[, names(X) %in% c("UH", "JI", "JK")]))
    DF1RS <- rowSums(DF1)
    DF1 <- do.call(rbind, lapply(1:nrow(DF1), function(i) 100*(DF1[i, ]/DF1RS[i])))
    FOR <- (rowSums(cbind(DF1$noun, DF1$article, DF1$adj, DF1$prep)) - 
        rowSums(cbind(DF1$pronoun, DF1$verb, DF1$adverb, DF1$interj)) + 100)/2
    if (!gv) {
        WOR <- sapply(split(WOR, grouping.var), sum, na.rm = TRUE)
    } else {
        WOR <- sum(WOR, na.rm=TRUE)
    }
    if(!gv) {
        FOR <- data.frame(replace = X[, 1], word.count = WOR, formality = FOR)
        colnames(FOR)[1] <- G
    } else {
        FOR <- data.frame(replace = X[, 1], word.count = WOR, 
            formality = FOR)
        colnames(FOR)[1] <- G
    }
    FOR[, "formality"] <- round(FOR[, "formality"], digits = digits)
    if (!gv & sort.by.formality) {
        FOR <- FOR[order(-FOR$formality), ]
        rownames(FOR) <- NULL
    }
    if (!gv) {
        prop.by <- data.frame(var=names(WOR), 
            word.count = WOR, 
            apply(DF1, 2, round, digits = digits))
        freq.by <- data.frame(var=names(WOR), 
            word.count = WOR, DF2)
    } else {
        prop.by <- data.frame(var="all", 
            word.count = sum(WOR, na.rm = TRUE), DF1)
        freq.by <- data.frame(var="all", 
            word.count = sum(WOR, na.rm = TRUE), DF2)
    }
    colnames(prop.by)[1] <- colnames(freq.by)[1] <- colnames(FOR)[1]
    rownames(prop.by) <- rownames(freq.by) <- NULL
    o <- unclass(pos.list)
    o$form.freq.by <- freq.by
    o$form.prop.by <- prop.by
    dat <- reshape(freq.by,           
        direction="long",           
        varying=list(c(3:10)), 
        idvar= names(freq.by)[1:2],          
        timevar="pos",          
        v.names=c("freq"),        
        times =names(freq.by)[-c(1:2)])
    colnames(dat)[1] <- "grouping"
    dat[, "form.class"] <- rep(c("formal", "contectual"), each = nrow(dat)/2)
    dat <- dat[rep(seq_len(dim(dat)[1]), dat[, 4]), -4]
    dat[, "pos"] <- factor(dat[, "pos"], levels=unique(dat[, "pos"]))
    dat[, "form.class"] <- factor(dat[, "form.class"], 
        levels=unique(dat[, "form.class"]))
    row.names(dat) <- NULL
    o$pos.reshaped <- dat
    o$formality <- FOR
    if (plot) {
        suppressWarnings(require(ggplot2))
        suppressWarnings(require(gridExtra))
        YY <- ggplot(dat, aes(grouping,  fill=form.class)) + 
            geom_bar(position='fill') + 
            coord_flip() +  labs(fill=NULL) + 
            opts(title = "Percent Contextual-Formal", 
                legend.position = 'bottom') 
        XX <- ggplot(data=dat, aes(grouping,  fill=pos)) + 
            geom_bar(position='fill') + coord_flip() + 
            facet_grid(~form.class, scales="free", margins = TRUE) +
            scale_x_discrete(drop=F) +  labs(fill=NULL) + 
            opts(title = "Percent Parts of Speech By Contextual-Formal", 
                legend.position = 'bottom')
        names(FOR)[1] <- "grouping"
        ZZ <- ggplot(data=FOR, aes(grouping,  formality, size=word.count)) + 
            geom_point(colour="grey50") + coord_flip()+
            geom_text(aes(label = word.count), vjust = 2, size = 3, 
                position = "identity") +  labs(size="word count") + 
            opts(title = "F Measure (Formality)", legend.position = 'bottom') +
            geom_point(colour="red", shape=20, size=.5)  
        gridExtra::grid.arrange(YY, XX, ZZ, widths=c(.25, .45, .3), ncol=3)
    }
    class(o) <- "formality.measure"
    return(o)
}