#' Find words associated with a given word(s) or a phrase(s).
#' 
#' Find words associated with a given word(s) or a phrase(s).  Results can be
#' outpur as a networrk graph and/or wordcloud.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param match.string %% ~~Describe \code{match.string} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
#' @param network.graph %% ~~Describe \code{network.graph} here~~
#' @param wordcloud %% ~~Describe \code{wordcloud} here~~
#' @param cloud.colors %% ~~Describe \code{cloud.colors} here~~
#' @param nw.label.cex %% ~~Describe \code{nw.label.cex} here~~
#' @param nw.label.colors %% ~~Describe \code{nw.label.colors} here~~
#' @param nw.layout %% ~~Describe \code{nw.layout} here~~
#' @param nw.edge.color %% ~~Describe \code{nw.edge.color} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
word.associate <-
function(text.var, grouping.var = NULL, text.unit = "sentence", match.string, 
    extra.terms = NULL, target.exclude = NULL, stopwords = NULL, 
    network.plot = FALSE, wordcloud = FALSE, cloud.colors = c("black", "gray55"), 
    title.color = "blue", nw.label.cex = .8, title.padj = -4.5, 
    nw.label.colors = NULL, nw.layout = NULL, nw.edge.color = "gray90", 
    nw.label.proportional = TRUE, nw.title.padj = NULL, nw.title.location = NULL, 
    title.font = NULL, title.cex = NULL, nw.edge.curved = TRUE, 
    cloud.legend = NULL, cloud.legend.cex = .8, cloud.legend.location = c(-.03, 1.03), 
    nw.legend = NULL, nw.legend.cex = .8, nw.legend.location = c(-1.54, 1.41),
    legend.overide = FALSE, ...){
    if(is.null(nw.label.colors)) {
        nw.label.colors <- cloud.colors
    }
    if(is.null(cloud.colors)) {
        cloud.colors <- nw.label.colors
    }
    if(all(cloud.colors %in% c("black", "gray55")) &&
        length(cloud.colors) != length(nw.label.colors)) {
        cloud.colors <- nw.label.colors
    }
    if (!legend.overide) {
        if(is.null(cloud.legend)) {
            cloud.legend <- nw.legend
        }
        if(is.null(nw.legend)) {
            nw.legend <- cloud.legend
        }
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
    Terms2 <- qdap::stopwords(text.var, stopwords = NULL, unlist = TRUE, 
          strip = TRUE, unique = TRUE, names = FALSE)    
    TM2 <- lapply(match.string, function(x) term.find(Terms2, 
        mat = tolower(x)))
    match.string <- lapply(TM2, function(i) Terms2[i])
    if (!is.null(target.exclude)) {
        match.string <- lapply(match.string, function(x) 
            x[!x %in% unlist(tolower(target.exclude))])
    }
    match.string <- lapply(match.string, function(x) paste0(" ", x, " "))
    if (!is.null(extra.terms)) {
        TM3 <- lapply(extra.terms, function(x) term.find(Terms2, 
            mat = tolower(x)))
        TM3 <- lapply(TM3, function(i) Terms2[i])
        if (!is.null(target.exclude)) {
            TM3 <- lapply(TM3, function(x) 
                x[!x %in% unlist(tolower(target.exclude))])
        }
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
    if (!is.null(target.exclude)) {
        COLTERMS <- lapply(COLTERMS, function(x) x[!x %in% target.exclude])
    }
    if (!is.null(stopwords)) {
        check <- unique(unlist(COLTERMS)) %in% Trim(unlist(stopwords))
        if (any(check)) {
            stop("match.string word(s) match stopword(s)\n",
                "  overlap terms: ",
                 paste(unique(unlist(COLTERMS))[check], collapse = " "))
        }
    }
    ECOLTERMS <- NULL
    if (!is.null(extra.terms)) {
        UET <- unlist(extra.terms, recursive = FALSE)
        ECOLTERMS <- lapply(UET, function(x) term.find(Terms, mat = x))
        ECOLTERMS <- lapply(ECOLTERMS, function(i) Terms[i])
        if (!is.null(target.exclude)) {
            ECOLTERMS <- lapply(ECOLTERMS, function(x) x[!x %in% target.exclude])
        }
    }
    if (wordcloud | network.plot) {
        if (!is.null(extra.terms)) {
            nm <- length(match.string)
            enm <- length(UET)
            ratio <- enm/nm
            snm <- 1:nm
            WSEARCH <- lapply(1:nm , function(i) unlist(list(COLTERMS[i], 
                ECOLTERMS[seq(ratio-1, enm, by=ratio)[i]:seq(ratio, enm, 
                by=ratio)[i]]), recursive=F))
        } else {
            WSEARCH <- COLTERMS
        }
        if (!is.null(target.exclude)) {
            WSEARCH <- lapply(WSEARCH, function(x) x[!x %in% target.exclude])
        }
    }
    if (!is.null(ECOLTERMS)) {
        v <- (length(COLTERMS) + length(ECOLTERMS))/length(COLTERMS)
    } else {
        v <- length(WSEARCH)
    }
    if(v != length(cloud.colors)-1) {
        need <- (v - (length(cloud.colors)-1))
        a1 <- ifelse(need > 0, "add", "subtract")
        a2 <- ifelse(need > 0, "to", "from")
        a3 <- ifelse(abs(need) > 1, "s ", " ")
        stop(a1, " ", abs(need), " ", "color", a3, a2, " the length of cloud.colors/nw.label.colors")
    }
    if (length(cloud.colors) != length(nw.label.colors)) {
        stop("the length of cloud.colors and nw.label.colors are not equal")
    }
    word.as <- function(dat, stopwords, search_terms = WSEARCH,
        network.graph, wordcloud, cloud.colors, title.color, nw.label.cex, 
        nw.label.colors, nw.layout, nw.edge.color, LN, nw.label.proportional,
        ECOLTERMS, cloud.legend, cloud.legend.cex, cloud.legend.location, 
        nw.legend, nw.legend.cex, nw.legend.location, ...){  
        LIST <- lapply(LN, function(x) dat[dat[, x], 2:4])
        FUN <- function(x) {
            (nrow(x) > 1) & !is.null(x)
        }
        choosennames <- sapply(LIST, FUN)
        FUN2 <- function(x) {
            (nrow(x) > 0) & !is.null(x)
        }
        choosennames2 <- sapply(LIST, FUN2)
        LISTb <- lapply(LN, function(x) dat[dat[, x], 1:4])
        if (sum(sapply(LISTb, function(x) nrow(x))) == 0) {
            return(NULL)
        }
        lapply(seq_along(LISTb), function(i){
            names(LISTb[[i]])[2:3] <<- c(G, TU)
        })
        LISTb <- LISTb[!sapply(LISTb, function(x) nrow(x) == 0)]
        qn <- LIST[!sapply(LIST, function(x) nrow(x)) == 0]
        qn <- unlist(lapply(length(qn), function(i) qn[[i]][, "group"]))
        names(LIST) <- paste0(unique(na.omit(qn)),
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
        LIST2 <- LIST2[!sapply(LIST2, is.null)]
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
            an <-  which(substring(names(o), 1, 6) == "adjmat")
            ads <- lapply(an, function(i) o[[i]])
            lapply(seq_along(ads), function(i) {
                word.network.plot(ads[[i]], label.cex = nw.label.cex, 
                title.name = namesL2[[i]], layout = nw.layout, 
                edge.color = nw.edge.color, title.color =title.color,
                label.colors = nw.label.colors,
                log.labels = nw.label.proportional, 
                title.padj = nw.title.padj, edge.curved = nw.edge.curved,
                title.location = nw.title.location, 
                title.font = title.font, title.cex = title.cex,
                legend = nw.legend, legend.cex = nw.legend.cex, 
                legend.location = nw.legend.location, 
                target.words = WSEARCH[choosennames][[i]])
             })
        }
        if (wordcloud) {
            lapply(seq_along(freqlist), function(i) {
               suppressWarnings(trans.cloud(word.list = freqlist[[i]]$swl, 
               target.words = WSEARCH[choosennames2][[i]], stopwords = stopwords, 
               cloud.colors = cloud.colors, expand.target = FALSE,
               title.color = title.color, title.names = namesL1[[i]], 
               legend = cloud.legend, legend.cex = cloud.legend.cex, 
               legend.location = cloud.legend.location, ...))
            })
        }
        return(o)    
    }
    Zdat <- split(DF3, DF3$group)
    lapply(seq_along(Zdat), function(i) {rownames(Zdat[[1]]) <<- NULL})
    o2 <- lapply(seq_along(Zdat), function(i) word.as(dat = Zdat[[i]],  
        stopwords = stopwords, network.graph = network.graph,  
        wordcloud = wordcloud, ECOLTERMS = ECOLTERMS,
        cloud.colors = cloud.colors, title.color =title.color,
        nw.label.proportional = nw.label.proportional,  
        nw.label.cex = nw.label.cex, nw.label.colors = nw.label.colors,  
        nw.layout = nw.layout, nw.edge.color = nw.edge.color, 
        LN = LN, cloud.legend = cloud.legend, cloud.legend.cex = cloud.legend.cex, 
        cloud.legend.location = cloud.legend.location, 
        nw.legend = nw.legend, nw.legend.cex = nw.legend.cex, 
        nw.legend.location = nw.legend.location, ...))
    names(o2) <- names(Zdat)
    o2$DF <- DFsl
    o2$match.terms <- lapply(match.string, Trim)
    if (!is.null(extra.terms)) {
        o2$extra.terms <- lapply(TM3, Trim)
    }
    class(o2) <- "word_associate"  
    return(o2)
}