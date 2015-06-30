#' Find Associated Words
#' 
#' Find words associated with a given word(s) or a phrase(s).  Results can be
#' output as a network graph and/or wordcloud.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param match.string A list of vectors or vector of terms to associate in the 
#' text.
#' @param text.unit The text unit (either \code{"sentence"} or \code{"tot"}.  
#' This argument determines what unit to find the match string words within.  
#' For example if \code{"sentence"} is chosen the function pulls all text for 
#' sentences the match string terms are found in.
#' @param extra.terms Other terms to color beyond the match string.
#' @param target.exclude A vector of words to exclude from the 
#' \code{match.string}.
#' @param stopwords Words to exclude from the analysis.
#' @param network.plot logical.  If \code{TRUE} plots a network plot of the 
#' words.
#' @param wordcloud logical.  If \code{TRUE} plots a wordcloud plot of the 
#' words.
#' @param cloud.colors A vector of colors equal to the length of 
#' \code{match.string} +1.
#' @param title.color A character vector of length one corresponding to the 
#' color of the title.
#' @param nw.label.cex The magnification to be used for network plot labels 
#' relative to the current setting of cex.  Default is .8.
#' @param title.padj Adjustment for the title. For strings parallel to the axes, 
#' padj = 0 means right or top alignment, and padj = 1 means left or bottom 
#' alignment.
#' @param nw.label.colors A vector of colors equal to the length of 
#' \code{match.string} +1.
#' @param nw.layout layout types supported by igraph.  See 
#' \code{\link[igraph]{layout}}.
#' @param nw.edge.color A character vector of length one corresponding to the 
#' color of the plot edges.
#' @param nw.label.proportional logical.  If \code{TRUE} scales the network 
#' plots across grouping.var to allow plot to plot comparisons.
#' @param nw.title.padj Adjustment for the network plot title. For strings 
#' parallel to the axes, padj = 0 means right or top alignment, and padj = 1 
#' means left or bottom alignment.
#' @param nw.title.location On which side of the network plot (1=bottom, 2=left, 
#' 3=top, 4=right).
#' @param title.font The font family of the cloud title. 
#' @param title.cex Character expansion factor for the title. \code{NULL} and 
#' \code{NA} are equivalent to 1.0.
#' @param nw.edge.curved logical.  If \code{TRUE} edges will be curved rather than 
#' straight paths.
#' @param cloud.legend A character vector of names corresponding to the number of 
#' vectors in \code{match.string}.  Both \code{nw.legend} and \code{cloud.legend}
#' can be set separately; or one may be set and by default the other will assume 
#' those legend labels.  If the user does not desire this behavior use the 
#' \code{legend.override} argument.
#' @param cloud.legend.cex Character expansion factor for the  wordcloud legend. 
#' \code{NULL} and \code{NA} are equivalent to 1.0. 
#' @param cloud.legend.location The x and y co-ordinates to be used to position the 
#' wordcloud legend.  The location may also be specified by setting x to a 
#' single keyword from the list \code{"bottomright"}, \code{"bottom"}, 
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, 
#' \code{"topright"}, \code{"right"} and \code{"center"}. This places the legend on 
#' the inside of the plot frame at the given location. 
#' @param nw.legend A character vector of names corresponding to the number of 
#' vectors in \code{match.string}.  Both \code{nw.legend} and \code{cloud.legend}
#' can be set separately; or one may be set and by default the other will assume 
#' those legend labels.  If the user does not desire this behavior use the 
#' \code{legend.override} argument.
#' @param nw.legend.cex Character expansion factor for the  network plot legend. 
#' \code{NULL} and \code{NA} are equivalent to 1.0. 
#' @param nw.legend.location  The x and y co-ordinates to be used to position the 
#' network plot legend.  The location may also be specified by setting x to a 
#' single keyword from the list \code{"bottomright"}, \code{"bottom"}, 
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, 
#' \code{"topright"}, \code{"right"} and \code{"center"}. This places the legend 
#' on the inside of the plot frame at the given location. 
#' @param legend.override By default if legend labels are supplied to either 
#' \code{cloud.legend} or \code{nw.legend} may be set and if the other remains 
#' \code{NULL} it will assume the supplied vector to the previous legend 
#' argument.  If this behavior is not desired \code{legend.override} should be 
#' set to \code{TRUE}.
#' @param char2space Currently a road to nowhere.  Eventually this will allow 
#' the retention of characters as is allowed in \code{trans_cloud} already.
#' @param \dots Other arguments supplied to \code{\link[qdap]{trans_cloud}}.
#' @return Returns a list:
#' \item{word frequency matrices}{Word frequency matrices for each grouping 
#' variable.} 
#' \item{dialogue}{A list of dataframes for each word list (each vector supplied 
#' to \code{match.string}) and a final dataframe of all combined text units that 
#' contain any match string.} 
#' \item{match.terms}{A list of vectors of word lists (each vector supplied 
#' to \code{match.string}).} 
#' Optionally, returns a word cloud and/or a network plot of the text unit 
#' containing the \code{match.string} terms.
#' @seealso \code{\link[qdap]{trans_cloud}},
#' \code{\link[qdap]{word_network_plot}},
#' \code{\link[wordcloud]{wordcloud}},
#' \code{\link[igraph]{graph.adjacency}}
#' @export
#' @examples
#' \dontrun{
#' ms <- c(" I ", "you")
#' et <- c(" it", " tell", "tru")
#' out1 <- word_associate(DATA2$state, DATA2$person, match.string = ms, 
#'     wordcloud = TRUE,  proportional = TRUE, 
#'     network.plot = TRUE,  nw.label.proportional = TRUE, extra.terms = et,  
#'     cloud.legend =c("A", "B", "C"),
#'     title.color = "blue", cloud.colors = c("red", "purple", "gray70"))
#' 
#' #======================================
#' #Note: You don't have to name the vectors in the lists but I do for clarity
#' ms <- list(
#'     list1 = c(" I ", " you", "not"), 
#'     list2 = c(" wh")          
#' )
#' 
#' et <- list(
#'     B = c(" the", "do", "tru"), 
#'     C = c(" it", " already", "we")
#' )
#' 
#' out2 <- word_associate(DATA2$state, DATA2$person, match.string = ms, 
#'     wordcloud = TRUE,  proportional = TRUE, 
#'     network.plot = TRUE,  nw.label.proportional = TRUE, extra.terms = et,  
#'     cloud.legend =c("A", "B", "C", "D"),
#'     title.color = "blue", cloud.colors = c("red", "blue", "purple", "gray70"))
#' 
#' out3 <- word_associate(DATA2$state, list(DATA2$day, DATA2$person), match.string = ms)
#' 
#' #======================================
#' m <- list(
#'     A1 = c("you", "in"), #list 1
#'     A2 = c(" wh")        #list 2
#' )
#' 
#' n <- list(
#'     B = c(" the", " on"), 
#'     C = c(" it", " no")
#' )
#' 
#' out4 <- word_associate(DATA2$state, list(DATA2$day, DATA2$person), 
#'     match.string = m)
#' out5 <- word_associate(raj.act.1$dialogue, list(raj.act.1$person), 
#'     match.string = m)
#' out6 <- with(mraja1spl, word_associate(dialogue, list(fam.aff, sex), 
#'      match.string = m))
#' names(out6)
#' lapply(out6$dialogue, htruncdf, n = 20, w = 20)
#' 
#' #======================================
#' DATA2$state2 <- space_fill(DATA2$state, c("is fun", "too fun"))
#' 
#' ms <- list(
#'     list1 = c(" I ", " you", "is fun", "too fun"), 
#'     list2 = c(" wh")      
#' )
#' 
#' et <- list(
#'     B = c(" the", " on"), 
#'     C = c(" it", " no")
#' )
#' 
#' out7 <- word_associate(DATA2$state2, DATA2$person, match.string = ms, 
#'     wordcloud = TRUE,  proportional = TRUE, 
#'     network.plot = TRUE,  nw.label.proportional = TRUE, extra.terms = et,  
#'     cloud.legend =c("A", "B", "C", "D"),
#'     title.color = "blue", cloud.colors = c("red", "blue", "purple", "gray70"))
#'     
#' DATA2 <- qdap::DATA2
#' }
word_associate <-
function(text.var, grouping.var = NULL, match.string, text.unit = "sentence",  
    extra.terms = NULL, target.exclude = NULL, stopwords = NULL, 
    network.plot = FALSE, wordcloud = FALSE, cloud.colors = c("black", "gray55"), 
    title.color = "blue", nw.label.cex = .8, title.padj = -4.5, 
    nw.label.colors = NULL, nw.layout = NULL, nw.edge.color = "gray90", 
    nw.label.proportional = TRUE, nw.title.padj = NULL, nw.title.location = NULL, 
    title.font = NULL, title.cex = NULL, nw.edge.curved = TRUE, 
    cloud.legend = NULL, cloud.legend.cex = .8, cloud.legend.location = c(-.03, 1.03), 
    nw.legend = NULL, nw.legend.cex = .8, nw.legend.location = c(-1.54, 1.41),
    legend.override = FALSE, char2space = "~~", ...){
    network.graph <- NULL
    if (is.list(match.string) & length(match.string) == 1) {
        match.string <- unlist(match.string)
    }
    if (!is.null(extra.terms) && 
        is.list(extra.terms) & length(extra.terms) == 1) {
        extra.terms <- unlist(extra.terms)
    }  
    if (network.plot | wordcloud) {
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
        if (!legend.override) {
            if(is.null(cloud.legend)) {
                cloud.legend <- nw.legend
            }
            if(is.null(nw.legend)) {
                nw.legend <- cloud.legend
            }
        }
    }
    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    if (!is.list(match.string)) {
        match.string <- list(match.string)
    }
    Terms2 <- rm_stopwords(text.var, stopwords = NULL, unlist = TRUE, 
        strip = TRUE, unique = TRUE, names = FALSE, char.keep = char2space)  
    if (!is.null(char2space)) {
        Terms2 <- mgsub(char2space, " ", Terms2)
    } 
    TM2 <- lapply(match.string, function(x) {
        term.find(Terms2, mat = tolower(x), unlist = TRUE)
    })
    match.string <- lapply(TM2, function(i) Terms2[i])
    if (!is.null(target.exclude)) {
        match.string <- lapply(match.string, function(x) 
            x[!x %in% unlist(tolower(target.exclude))])
    }
    match.string <- spaste(match.string)
    if (!is.null(extra.terms)) {
        TM3 <- lapply(extra.terms, function(x) {
            term.find(Terms2, mat = tolower(x), unlist = TRUE)
        })
        TM3 <- lapply(TM3, function(i) Terms2[i])
        if (!is.null(target.exclude)) {
            TM3 <- lapply(TM3, function(x) 
                x[!x %in% unlist(tolower(target.exclude))])
        }
    }
    suppressWarnings(if(is.null(text.unit)) {
        TU <- "row"
    } else {
        if (is.list(text.unit)) {
            m <- unlist(as.character(substitute(text.unit))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
                TU <- paste(m, collapse="&")
        } else {
            if (is.vector(text.unit) & length(text.unit) == 1 & 
                text.unit == "sentence") {
                    TU <- "sentence"
            } else {
                TU <- as.character(substitute(text.unit))
                TU <- TU[length(TU)]
            }
        }
    })
    if(is.null(text.unit)){
        texting <- as.factor(1:length(text.var))
    } else {
        if(is.list(text.unit) & length(text.unit)>1) {
            texting <- apply(data.frame(text.unit), 1, function(x){
                if(any(is.na(x))){
                    NA
                }else{
                    paste(x, collapse = ".")
                }
            })
        } else {
            if (TU == "tot") {
                texting <- sapply(strsplit(as.character(text.unit), ".", 
                    fixed=TRUE), function(x) x[[1]])
            } else {
                if (TU %in% c("sentence", "sent")) {
                    texting <- as.factor(1:length(text.var))
                } else {
                    texting <- unlist(text.unit)
                }
            }
        } 
    } 
    text.var2 <- text.var
    if (!is.null(char2space)) {
        text.var <- mgsub(char2space, " ", text.var)
    }
    DF <- data.frame(row = seq_along(text.var), group = grouping, unit = texting,
        text = as.character(text.var), stringsAsFactors = FALSE)    
    LOG <- lapply(match.string, function(x) {
            apply(term.find(tolower(DF$text), mat = x, logic = TRUE), 1, any)
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
    DFsl <- lapply(ALN, function(i) stats::na.omit(DF3[shortDF(DF3, i), 1:4]))
    names(DFsl) <- colnames(DF3)[-c(1:4)]
    Terms <- rm_stopwords(text.var2, stopwords = NULL, unlist = TRUE, 
          strip = TRUE, unique = TRUE, names = FALSE, char.keep = char2space)
    if (!is.null(char2space)) {
        Terms <- mgsub(char2space, " ", Terms)
    }       
    TM <- lapply(match.string, function(x) term.find(Terms, 
        mat = x, unlist = TRUE))
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
    if ((!wordcloud & !network.plot) & !is.null(extra.terms)) {
        extra.terms <- NULL
        warning("wordcloud and network.plot are FALSE; extra.terms ignored")
    }
    if (!is.null(extra.terms)) {
        UET <- unlist(extra.terms, recursive = FALSE)
        ECOLTERMS <- lapply(UET, function(x) term.find(Terms, mat = x, ))
        ECOLTERMS <- lapply(ECOLTERMS, function(i) {
            if (identical(unlist(i), integer(0))) return(NULL)         
            Terms[i]
        })
        if (!is.null(target.exclude)) {
            ECOLTERMS <- lapply(ECOLTERMS, function(x) x[!x %in% target.exclude])
        }
        if (is.list(extra.terms) & length(extra.terms) > 1){
            ECOLTERMS <- lapply(names(extra.terms), function(x) {
                z <- unlist(ECOLTERMS[grepl(x, names(ECOLTERMS), fixed = TRUE)])
                names(z) <- NULL
                z
            })
            names(ECOLTERMS) <- names(extra.terms)
        }
        if (!is.list(extra.terms)) {
             ECOLTERMS <- list(unlist(ECOLTERMS))
        }
    }
    DF4 <- DF3
    DF4$text <- text.var2
    if (wordcloud | network.plot) {
        if (!is.null(extra.terms)) {
            nm <- length(match.string)
            enm <- length(UET)
            ratio <- enm/nm
            snm <- 1:nm
            WSEARCH <- lapply(1:nm , function(i) {
                z <- list(COLTERMS[i], ECOLTERMS)
                unlist(z, recursive=F)
            })
        } else {
            WSEARCH <- COLTERMS
        }
        if (!is.null(target.exclude)) {
            WSEARCH <- lapply(WSEARCH, function(x) x[!x %in% target.exclude])
        }
        if (!is.null(ECOLTERMS)) {
            v <- 1 + length(ECOLTERMS)
        } else {
            v <- 1
        }
    }
    if (wordcloud | network.plot) {
        if (v != length(cloud.colors)-1) {
            need <- (v - (length(cloud.colors)-1))
            a1 <- ifelse(need > 0, "add", "subtract")
            a2 <- ifelse(need > 0, "to", "from")
            a3 <- ifelse(abs(need) > 1, "s ", " ")
            stop(a1, " ", abs(need), " ", "color", a3, a2, 
                " the length of cloud.colors/nw.label.colors")
        }
        if (length(cloud.colors) != length(nw.label.colors)) {
            stop("the length of cloud.colors and nw.label.colors are not equal")
        }
    }
    word.as <- function(dat, stopwords, search_terms = WSEARCH,
        network.graph, wordcloud, cloud.colors, title.color, nw.label.cex, 
        nw.label.colors, nw.layout, nw.edge.color, LN, nw.label.proportional,
        ECOLTERMS, cloud.legend, cloud.legend.cex, cloud.legend.location, 
        nw.legend, nw.legend.cex, nw.legend.location, char.keep, char2space, ...){  
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
        names(LIST) <- paste0(unique(stats::na.omit(qn)),
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
                    stopwords = stopwords, char2space = char2space)
            }
        )
        mats2 <- lapply(mats, function(x) {
                adjacency_matrix(t(x))
            }
        )
        freqlist <- lapply(LIST, function(x) {
            word_list(x$text, stopwords = stopwords, char.keep = char2space)
        })
        o <- list(list = LISTb, search.terms = COLTERMS, freqlist = freqlist, 
            freqmat = mats, adjmat = mats2)
        if(!is.null(ECOLTERMS)) {
            o[["extra.terms"]] <- ECOLTERMS
        }
        o <- unlist(o, recursive = FALSE)
        if (network.plot) {
            an <-  grep("adjmat", names(o))
            ads <- lapply(an, function(i) o[[i]])
            invisible(lapply(seq_along(ads), function(i) {
                word_network_plot(ads[[i]], label.cex = nw.label.cex, 
                title.name = namesL2[[i]], layout = nw.layout, 
                edge.color = nw.edge.color, title.color =title.color,
                label.colors = nw.label.colors,
                log.labels = nw.label.proportional, 
                title.padj = nw.title.padj, edge.curved = nw.edge.curved,
                title.location = nw.title.location, 
                title.font = title.font, title.cex = title.cex,
                legend = nw.legend, legend.cex = nw.legend.cex, 
                legend.location = nw.legend.location, 
                char2space = char2space, 
                target.words = WSEARCH[choosennames][[i]])
             }))
        }
        if (wordcloud) {
            invisible(lapply(seq_along(freqlist), function(i) {
               suppressWarnings(trans_cloud(
                   word.list = freqlist[[i]]$swl, 
                   target.words = WSEARCH[choosennames2][[i]], 
                   stopwords = stopwords, 
                   cloud.colors = cloud.colors, expand.target = FALSE,
                   title.color = title.color, title.names = namesL1[[i]], 
                   legend = cloud.legend, legend.cex = cloud.legend.cex, 
                   char2space = char2space, char.keep = char2space, 
                   legend.location = cloud.legend.location, ...))
            }))
        }
        return(o)    
    }
    Zdat <- split(DF4, DF3$group)
    invisible(lapply(seq_along(Zdat), function(i) {rownames(Zdat[[1]]) <<- NULL}))
    o2 <- lapply(seq_along(Zdat), function(i) word.as(dat = Zdat[[i]],  
        stopwords = stopwords, network.graph = network.graph,  
        wordcloud = wordcloud, ECOLTERMS = ECOLTERMS,
        cloud.colors = cloud.colors, title.color =title.color,
        nw.label.proportional = nw.label.proportional, char2space = char2space,  
        nw.label.cex = nw.label.cex, nw.label.colors = nw.label.colors,  
        nw.layout = nw.layout, nw.edge.color = nw.edge.color, 
        LN = LN, cloud.legend = cloud.legend, cloud.legend.cex = cloud.legend.cex, 
        cloud.legend.location = cloud.legend.location, 
        nw.legend = nw.legend, nw.legend.cex = nw.legend.cex, 
        nw.legend.location = nw.legend.location, ...))
    names(o2) <- names(Zdat)
    o2$dialogue <- DFsl
    o2$match.terms <- lapply(match.string, Trim)
    if (!is.null(extra.terms)) {
        o2$extra.terms <- lapply(TM3, Trim)
    }
    class(o2) <- "word_associate"  
    return(o2)
}

#' Prints a word_associate object
#' 
#' Prints a word_associate object.
#' 
#' @param x The word_associate object
#' @param \ldots ignored
#' @method print word_associate
#' @export
print.word_associate <-
function(x, ...) {
    elem <- unlist(x, recursive=FALSE)
    wid <- options()$width
    options(width = 10000)
    print(left_just(elem$dialogue.any, 4))
    message("\nMatch Terms\n===========")
    invisible(lapply(seq_along(x$match.terms), function(i) {
      message(paste0("\n", "List ", i, ":\n", paste(x$match.terms[[i]], 
          collapse = ", ")))
    }))
     message("\n")
    options(width = wid) 
}
