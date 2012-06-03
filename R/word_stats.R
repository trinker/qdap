#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
word_stats <-
function(text.var, grouping.var = NULL, tot = NULL,
    digit.remove = FALSE, apostrophe.remove = FALSE) {

        G <- if(is.null(grouping.var)) {
            "all"
        } else {
            if (is.list(grouping.var)) {
                m <- unlist(as.character(substitute(grouping.var))[-1])
                m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                        x[length(x)]
                    }
                )
                paste(m, collapse="&")
            } else {
                G <- as.character(substitute(grouping.var))
                G[length(G)]
            }
        }
    grouping <- if(is.null(grouping.var)){
        rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            apply(data.frame(grouping.var), 1, function(x){
                    if (any(is.na(x))){
                        NA
                    } else {
                        paste(x, collapse = ".")
                    }
                }
            )
        } else {
            unlist(grouping.var)
        } 
    } 
    t.o.t. <- if(is.null(tot)){
        paste0(1:length(text.var), ".", 1)
    } else {
        as.character(tot)
    }
    Text <- as.character(text.var)
    DF <- na.omit(data.frame(group = grouping, tot.sen = t.o.t., 
        TOT = TOT(t.o.t.), text.var = Text, stringsAsFactors = FALSE))
    DF$group <- DF$group[ , drop=TRUE]
    DF$n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(DF$group, DF$n.sent)), ]
    M<-DF_word_stats(text.var = DF$text.var, digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove)
    M <- M[, !names(M) %in% c("text.var", "n.sent")]
    DF <- data.frame(DF, M)
    DF$end.mark <- substring(DF$text.var, nchar(DF$text.var), 
        nchar(DF$text.var))
    DF$end.mark2 <- substring(DF$text.var, nchar(DF$text.var)-1, 
        nchar(DF$text.var))
    DF$sent.type <- ifelse(DF$end.mark2%in%c("*.", "*?", "*!"), "imperative",
        ifelse(DF$end.mark==".", "statement", 
        ifelse(DF$end.mark=="?", "question",
        ifelse(DF$end.mark=="!", "exclamation",
        ifelse(DF$end.mark=="|", "interupted", NA))))) 
    DF$end.mark2 <- NULL
    DF2 <- aggregate(word.count ~ group, DF, sum)
    names(DF2) <- c("group", "n.words")
    totter <- function(x)length(unique(x))
    DF2$n.tot <- aggregate(TOT ~ group, DF, totter)$TOT
    DF2$n.sent <- as.data.frame(table(DF$group))$Freq
    DF2$n.char <- aggregate(character.count ~ 
        group, DF, sum)$character.count
    DF2$n.syl <- aggregate(syllable.count ~ 
        group, DF, sum)$syllable.count
    DF2$n.poly <- aggregate(polysyllable.count ~ 
        group, DF, sum)$polysyllable.count
    DF2 <- DF2[,c("group", "n.tot", "n.sent", "n.words", 
        names(DF2)[-c(1:4)])]
    DF2 <- transform(DF2, sptot = round(n.sent/n.tot, digits=3),
        wps = round(n.words/n.sent, digits=3),
        cps = round(n.char/n.sent, digits=3),
        sps = round(n.syl/n.sent, digits=3),
        psps = round(n.poly/n.sent, digits=3),
        cpw = round(n.char/n.words, digits=3),
        spw = round(n.syl/n.words, digits=3),
        pspw = round(n.poly/n.words, digits=3))
    DF2$n.state <- aggregate(sent.type ~ group, DF, function(x) 
        length(which(x=="statement")))$sent.type
    DF2$n.quest <- aggregate(sent.type ~ group, DF, function(x) 
        length(which(x=="question")))$sent.type
    DF2$n.exclm <- aggregate(sent.type ~ group, DF, function(x) 
        length(which(x=="exclamation")))$sent.type
    DF2$n.imper <- aggregate(sent.type ~ group, DF, function(x) 
        length(which(x=="imperative")))$sent.type
    X <- aggregate(end.mark ~ group, DF, function(x) 
        length(which(x=="interupted")))$end.mark
    DF2$incomplete <- if(sum(X)==0) NULL else X
    DF2 <- DF2[order(-DF2$n.words), ]
    qdaMOD <-if(is.null(grouping.var)){
        DFfreq <- data.frame(table(unlist(
        word.split(strip(text.var)))))
        names(DFfreq) <- c('WORD', 'FREQ')
        list(fwl=list(all=DFfreq))
    } else {
        qda(DF[, 'text.var'], DF[, 'group'], cut.n = 10)
    }
    DIS <- unlist(lapply(qdaMOD$fwl, function(x) sum(x[,2]==2)))
    HAPAX <- unlist(lapply(qdaMOD$fwl, function(x) sum(x[,2]==1)))
    ALL <- unlist(lapply(qdaMOD$fwl, function(x) sum(x[,2])))
    rankDF <- data.frame(words=ALL, group=names(DIS), 
        n.hapax=HAPAX, n.dis=DIS, grow.rate=round(HAPAX/ALL, 
        digits=3), prop.dis= round(DIS/ALL, digits=3))
    rankDF <- rankDF[order(-rankDF$words),]
    rownames(rankDF) <- 1:nrow(rankDF)
    DF2 <- data.frame(DF2, rankDF[, -c(1:2)])
    names(DF2) <- c(G, names(DF2)[-1])
    DF2 <- DF2[order(-DF2$n.tot, -DF2$n.sent), ]
    rownames(DF2) <- NULL
    DF3 <- DF
    DF3 <- DF3[order(DF3$n.sent), ]
    rownames(DF3) <- NULL
    names(DF3) <- c(G, names(DF3)[c(2:3)], "text.var", 
        "sent.num", names(DF3)[-c(1:5)])
    DF3$tot.sen <- if(is.null(tot)){
        NULL
    } else {
        DF3$tot.sen 
    }
    DF3$TOT <- if (is.null(tot)){
        NULL
    } else {
        DF3$TOT
    }
    DF2$n.tot <- if(is.null(tot)){
        NULL
    } else {
        DF2$n.tot
    } 
    DF2$sptot <- if(is.null(tot)){
        NULL
    } else {
        DF2$sptot
    }

    if(sum(DF2$n.imper, na.rm = TRUE)==0) DF2$n.imper <- NULL
    o <- list(ts= DF3, gts = DF2)
    class(o) <- "word.stats"
    return(o)
}
