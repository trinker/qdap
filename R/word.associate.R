word.associate <-
function(text.var, grouping.var = NULL, match.string, 
    stopwords = NULL, network.graph = FALSE, wordcloud = FALSE, 
    cloud.colors = c("black", "gray"), nw.label.cex =.8, 
    nw.label.colors = c("blue", "gray65"), nw.layout = NULL, 
    nw.edge.color = "gray90", ...){
    if (!is.list(match.string)) {
        match.string <- list(match.string)
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
        as.factor(1:length(text.var))
    } else {
        if(is.list(grouping.var) & length(grouping.var)>1) {
            apply(data.frame(grouping.var), 1, function(x){
                if(any(is.na(x))){
                    NA
                }else{
                    paste(x, collapse = ".")
                     }
                }
            )
        } else {
            if (G == "tot") {
                sapply(strsplit(as.character(grouping.var), ".", 
                    fixed=TRUE), function(x) x[[1]])
            } else {
                unlist(grouping.var)
            }
        } 
    } 
    DF <- data.frame(group = grouping, text = as.character(text.var), 
        stringsAsFactors = FALSE)
    DF <- na.omit(DF)    
    LIST <- split(DF, DF$group)
    collapse <-function(x, sep = " ") {
        paste(x, collapse=sep)
    }
    LIST2 <- lapply(seq_along(LIST), function(i) collapse(LIST[[i]][, "text"]))
    DF <- data.frame(group = names(LIST), text = do.call("rbind", LIST2))
    if (G %in% c("all", "tot")) {
        DF$group <- as.numeric(as.character(DF$group))
    }
    locs <- term.find(str =  DF$text, mat = match.string)
    RET2 <- RET <- lapply(locs, function(x) {DF[x, ]})
    names(RET) <- name <- lapply(match.string, 
        function(x) collapse(capitalizer(strip(x)), "_"))
    mats <- lapply(RET2, function(x) {
           wfm(grouping.var = Trim(x[, "group"]), text.var = x[, "text"],
                stopwords = stopwords)
        }
    )
    mats2 <- lapply(mats, function(x) {
            adjacency_matrix(t(x))
        }
    )
    lapply(seq_along(RET), function(i){
            names(RET[[i]])[1] <<- ifelse(G == "all", "sentences", G)
        }
    )
    ord <- function(x) x[order(x[, 1]), ]
    lapply(seq_along(RET), function(i){
            RET[[i]] <<- ord(RET[[i]])
        }
    )
    freqlist <- lapply(RET, function(x) qda(x$text, stopwords = stopwords))
    name <- strsplit(as.character(name), "_", fixed=TRUE)
    o <- unlist(list(obs = RET, search.terms = name, freqlist = freqlist, 
        freqmat = mats, adjmat = mats2), recursive = FALSE)
    if (!is.null(stopwords)) {
        o[["stopwords"]] <- stopwords
        check <- Trim(unlist(match.string)) %in% Trim(unlist(stopwords))
        if (any(check)) {
            stop("match.string word(s) match stopword(s)\n",
                "  use $warning to see overlap words")
            o[["warning"]] <- Trim(unlist(match.string))[check]
        }
    }
    class(o) <- "word_associate"    
    if (network.graph) {
        require(igraph)
        an <-  which(substring(names(o), 1, 6) == "adjmat")
        ads <- lapply(an, function(i) o[[i]][["adjacency"]])
        gigraph <- function(am, mat, nw.label.cex, nw.edge.col, nw.label.cols, 
            nw.layout) {
            g <- igraph::graph.adjacency(am, weighted=TRUE, mode ='undirected') 
            g <- igraph::simplify(g)
            igraph::V(g)$label <- igraph::V(g)$name
            igraph::V(g)$degree <- igraph::degree(g)
            igraph::V(g)$label.cex <- nw.label.cex
            V(g)$label.color <- ifelse(V(g)$label %in% Trim(mat), nw.label.cols[1],
                nw.label.cols[2])
            E(g)$color <- nw.edge.col
            if (is.null(nw.layout)) {
                nw.layout <- igraph::layout.fruchterman.reingold(g)
            }
            if (dev.interactive()) dev.new()
            plot(g, layout=nw.layout, vertex.size=0, vertex.color="white")
        }
    lapply(seq_along(ads), function(i) gigraph(ads[[i]], 
        nw.label.cex = nw.label.cex, 
        nw.layout = nw.layout, nw.edge.col = nw.edge.color, 
        nw.label.cols = nw.label.colors, mat[[i]]))
    }
    if (wordcloud) {
        lapply(seq_along(freqlist), function(i) trans.cloud(
           word.list = freqlist[[i]]$swl, target.words = name[[i]], 
           stopwords = stopwords, cloud.colors = cloud.colors,
           ...))
    }
    return(o)    
}
