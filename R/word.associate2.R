word.associate2 <-
function(text.var, grouping.var = NULL, text.unit = "sentence", match.string, 
    extra.terms = NULL, stopwords = NULL, network.plot = FALSE, wordcloud = FALSE, 
    cloud.colors = c("black", "gray"), title.color = "blue", nw.label.cex =.8, 
    title.padj = title.padj, nw.label.colors = NULL, nw.layout = NULL, 
    nw.edge.color = "gray90", nw.title.padj = NULL, nw.title.location = NULL, 
    title.font = NULL, title.cex = NULL, ...){
    if(is.null(nw.label.colors)) {
        nw.label.colors <- cloud.colors
    }
    if(is.null(cloud.colors)) {
        cloud.colors <- nw.label.colors
    }
    G <- if(is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
                paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    grouping <- if(is.null(grouping.var)){
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
    if (!is.list(match.string)) {
        match.string <- list(match.string)
    }
    TU <- suppressWarnings(if(is.null(text.unit)) {
        "row"
    } else {
        if (is.list(text.unit)) {
            m <- unlist(as.character(substitute(text.unit))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
                paste(m, collapse="&")
        } else {
            if (is.vector(text.unit) & length(text.unit) == 1 & 
                text.unit == "sentence") {
                    "sentence"
            } else {
                TU <- as.character(substitute(text.unit))
                TU[length(TU)]
            }
        }
    })
    texting <- if(is.null(text.unit)){
        as.factor(1:length(text.var))
    } else {
        if(is.list(text.unit) & length(text.unit)>1) {
            apply(data.frame(text.unit), 1, function(x){
                if(any(is.na(x))){
                    NA
                }else{
                    paste(x, collapse = ".")
                     }
                }
            )
        } else {
            if (TU == "tot") {
                sapply(strsplit(as.character(text.unit), ".", 
                    fixed=TRUE), function(x) x[[1]])
            } else {
                if (TU %in% c("sentence", "sent")) {
                    as.factor(1:length(text.var))
                } else {
                    unlist(text.unit)
                }
            }
        } 
    } 
    DF <- data.frame(row = seq_along(text.var), group = grouping, unit = texting,
        text = as.character(text.var), stringsAsFactors = FALSE)    
    LOG <- lapply(match.string, function(x) {
            apply(term.find(DF$text, mat = x, logic = TRUE), 1, any)
        } 
    )
    DF2 <- data.frame(do.call("cbind", LOG))
    names(DF2) <- paste0("list", 1:ncol(DF2))
    DF2[, "any"] <- apply(DF2, 1, any)
    DF3 <- data.frame(DF, DF2)
    LDF <- lapply(4:ncol(DF3), function(i) DF3[DF3[, i], 1:3])
    names(LDF) <- names(DF3)[5:ncol(DF3)]
    ALN <- 5:ncol(DF3)
    LN <- 5:c(ncol(DF3)-1)
    shortDF <- function(dat, col) {
        unlist(lapply(split(dat, dat[, 3]), function(x) {
            rep(any(x[, col]), nrow(x))
        }))
    }
    DFsl <- lapply(ALN, function(i) na.omit(DF3[shortDF(DF3, i), 1:4]))
    names(DFsl) <- colnames(DF3)[-c(1:4)]
    Terms <- qdap::stopwords(text.var, stopwords = NULL, unlist = TRUE, 
          strip = TRUE, unique = TRUE, names = FALSE)     
    TM <- lapply(match.string, function(x) term.find(Terms, 
        mat = x))
    COLTERMS <- lapply(TM, function(i) Terms[i])
    if (!is.null(stopwords)) {
        check <- unique(unlist(COLTERMS)) %in% Trim(unlist(stopwords))
        if (any(check)) {
            stop("match.string word(s) match stopword(s)\n",
                "  overlap terms: ",
                 paste(unique(unlist(COLTERMS))[check], collapse = " "))
        }
    }  
    if (!is.null(extra.terms)) {
        if (!is.list(extra.terms)) {
            extra.terms <- list(extra.terms)
        }
        ETM <- lapply(extra.terms, function(x) term.find(Terms, 
            mat = x))
        ECOLTERMS <- lapply(ETM, function(i) Terms[i])
        if (length(COLTERMS) != length(ECOLTERMS) & length(ECOLTERMS) == 1) {
            ECOLTERMS <- rep(ECOLTERMS, length(COLTERMS))
        } else { 
            if (length(COLTERMS) != length(ECOLTERMS) & length(ECOLTERMS) != 1) {
                stop("length of extra.terms must be equal to 1 or equal to match.string")
            }
        }
#         if (length(cloud.colors) != length(nw.label.colors) |
#             length(cloud.colors) != sum(length(COLTERMS), 
#             length(ECOLTERMS))) {
#                 cloud.colors <- nw.label.colors <- rep("black",
#                     sum(length(COLTERMS), length(ECOLTERMS)))
#                 warning(paste("wrong number of nw.label.colors and/or cloud colors",
#                 "\nblack will be used"))
#         }
    } else {
        ECOLTERMS <- NULL
    }
    if (!is.null(ECOLTERMS)) {
        WSEARCH <- lapply(seq_along(COLTERMS), function(i) 
            list(unlist(COLTERMS[[i]]), unlist(ECOLTERMS[[i]])))
    } else {
        WSEARCH <- COLTERMS
    }
    word.as <- function(dat, stopwords, search_terms = WSEARCH,
        network.graph, wordcloud, cloud.colors, title.color, nw.label.cex, 
        nw.label.colors, nw.layout, nw.edge.color, LN, ...){  
        LIST <- lapply(LN, function(x) dat[dat[, x], 2:4])
        LISTb <- lapply(LN, function(x) dat[dat[, x], 1:4])
        if (sum(sapply(LISTb, function(x) nrow(x))) == 0) {
            return(NULL)
        }
        lapply(seq_along(LISTb), function(i){
            names(LISTb[[i]])[2:3] <<- c(G, TU)
        })
        LISTb <- LISTb[!sapply(LISTb, function(x) nrow(x) == 0)]
        names(LIST) <- paste0(unique(na.omit(LIST[[1]][, "group"])),
            "_list", seq_along(LIST))
        LIST <- LIST[!sapply(LIST, is.null)]
        LIST <- LIST[!sapply(LIST, function(x) nrow(x) == 0)]
        namesL1 <- names(LIST)
        LIST2 <- LIST[sapply(LIST, function(x) nrow(x) > 1)]
        collapse <-function(x, sep = " ") {
            paste(x, collapse=sep)
        }
        collap <- function(DF){ 
            SP <- split(DF, DF[, "unit"])
            SP2 <- lapply(SP, function(x) collapse(x[, "text"]))
            if (length(SP2) > 1) {
                SP3 <- do.call("rbind", SP2)
                return(data.frame(unit = rownames(SP3), text = SP3,
                    stringsAsFactors = FALSE))
            } else {
                return(NULL)
            }
        }
        if (!TU %in% c("sentence", "row")){
            LIST2 <- lapply(LIST2, collap)
        } else {
            LIST2 <- lapply(LIST2, function(x) x[, 2:3]) 
        }
        namesL2 <- names(LIST2)
        mats <- lapply(LIST2, function(x) {
               wfm(grouping.var = Trim(x[, "unit"]), text.var = x[, "text"],
                    stopwords = stopwords)
            }
        )
        mats2 <- lapply(mats, function(x) {
                adjacency_matrix(t(x))
            }
        )
        freqlist <- lapply(LIST, function(x) {
            qda(x$text, stopwords = stopwords)
        })
        o <- list(list = LISTb, search.terms = COLTERMS, freqlist = freqlist, 
            freqmat = mats, adjmat = mats2)
        if(!is.null(ECOLTERMS)) {
            o[["extra.terms"]] <- ECOLTERMS
        }
        o <- unlist(o, recursive = FALSE)
        if (network.plot) {
            text2color <- function(text, words, colors, nomatch) {
                lookup <- lapply(seq_along(words), function(n) cbind(words[[n]], 
                    colors[n]))
                lookup <- do.call("rbind.data.frame", lookup)
                lookup <- apply(lookup, 2, as.character)
                recode <- lookup[match(text, lookup[, 1]), 2]
                recode[is.na(recode)] <- nomatch
                return(recode)
            }
            require(igraph)
            an <-  which(substring(names(o), 1, 6) == "adjmat")
            ads <- lapply(an, function(i) o[[i]][["adjacency"]])
            gigraph <- function(am, mat, nw.label.cex, nw.edge.col, nw.label.cols, 
                nw.layout, text, nw.title.padj, side, title.font, title.cex, COLTERMSi) {
                g <- igraph::graph.adjacency(am, weighted=TRUE, mode ='undirected') 
                g <- igraph::simplify(g)
                igraph::V(g)$label <- igraph::V(g)$name
                igraph::V(g)$degree <- igraph::degree(g)
                igraph::V(g)$label.cex <- nw.label.cex
                nwc <- length(nw.label.cols)
                COLORS <- text2color(V(g)$label, words = COLTERMSi, 
                    colors = nw.label.cols[-nwc], nomatch = nw.label.cols[nwc])
                V(g)$label.color <- COLORS
                E(g)$color <- nw.edge.col
                if (is.null(nw.layout)) {
                    nw.layout <- igraph::layout.fruchterman.reingold(g)
                }
                if (dev.interactive()) dev.new()
                plot(g, layout=nw.layout, vertex.size=0, vertex.color="white")
                if (is.null(nw.title.padj)){
                    nw.title.padj = -4.5
                }
                if (is.null(nw.title.location)){
                    nw.title.location = 3
                }
                if (!is.null(title.color)) {
                    mtext(text, side = nw.title.location, padj = nw.title.padj, 
                    col = title.color, family = title.font, cex = title.cex)
                }
            }
        lapply(seq_along(ads), function(i) gigraph(ads[[i]], 
            nw.label.cex = nw.label.cex, text = namesL2[[i]],
            nw.layout = nw.layout, nw.edge.col = nw.edge.color, 
            nw.label.cols = nw.label.colors, match.string[[i]],
            nw.title.padj = nw.title.padj, side = nw.title.location, 
            title.font = title.font, title.cex = title.cex, COLTERMSi = WSEARCH[[i]]))
        }
        if (wordcloud) {
            lapply(seq_along(freqlist), function(i) {
               trans.cloud(word.list = freqlist[[i]]$swl, target.words = WSEARCH[[i]], 
               stopwords = stopwords, cloud.colors = cloud.colors,
               title.color = title.color, title.names = namesL1[[i]], ...)
            })
        }
        return(o)    
    }
    Zdat <- split(DF3, DF3$group)
    lapply(seq_along(Zdat), function(i) {rownames(Zdat[[1]]) <<- NULL})
    o2 <- lapply(seq_along(Zdat), function(i) word.as(dat = Zdat[[i]],  
        stopwords = stopwords, network.graph = network.graph,  
        wordcloud = wordcloud,  
        cloud.colors = cloud.colors,  
        nw.label.cex = nw.label.cex, nw.label.colors = nw.label.colors,  
        nw.layout = nw.layout, nw.edge.color = nw.edge.color, title.color =title.color,
        LN = LN, ...))
    names(o2) <- names(Zdat)
    o2 <- unlist(o2, recursive=FALSE)
    o2$DF <- DFsl
    class(o2) <- "word_associate"  
    return(o2)
}
