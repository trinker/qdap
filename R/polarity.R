#' Polarity Score (Sentiment Analysis)
#' 
#' Approximate the sentiment (polarity) of text by grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param positive.list A character vector of terms indicating positive reaction.
#' @param negative.list A character vector of terms indicating negative reaction.
#' @param negation.list  A character vector of terms reversing the intent of a 
#' positive or negative word.
#' @param amplification.list  A character vector of terms that increases the 
#' intensity of a positive or negative word.
#' @param rm.incomplete logical.  If TRUE text rows ending with qdap's 
#' incomplete sentence end mark (\code{|}) will be removed from the analysis.
#' @param digits Integer; number of decimal places to round when printing. 
#' @param \ldots Other arguments supplied to \code{\link[qdap]{end_inc}}.
#' @return Returns a list of:
#' \item{all}{A dataframe of scores per row with:
#' \itemize{
#'   \item  group.var - the grouping variable
#'   \item  text.var - the text variable
#'   \item  wc - word count
#'   \item  polarity - sentence polarity score
#'   \item  raw - raw polarity score (considering only positive and negative 
#'   words)
#'   \item  negation.adj.raw - raw adjusted for negation words
#'   \item  amplification.adj.raw - raw adjusted for amplification words
#'   \item  pos.words - words considered positive
#'   \item  neg.words - words considered negative}
#' }
#' \item{group}{A dataframe with the average polarity score by grouping 
#' variable.}
#' \item{digits}{integer value od number of digits to display; mostly internal 
#' use} 
#' @seealso \url{https://github.com/trestletech/Sermon-Sentiment-Analysis}
#' @note The polarity score is dependent upon the polarity dictionary used.  
#' This function defaults to the word polarity word dictionary used by Hu, M., & 
#' Liu, B. (2004), however, this may not be appropriate for the context of 
#' children in a classroom.  The user may (is encouraged) to provide/augment the 
#' dictionary.  For instance the word "sick" in a high school setting may mean 
#' that something is good, whereas "sick" used by a typical adult indicates 
#' something is not right or negative connotation.
#' 
#' Also note that \code{\link[qdap]{polarity}} assumes you've run 
#' \code{\link[qdap]{sentSplit}}.
#' @details The equation used by the algorithm to assign value to polarity to 
#' each sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to 
#' tag each word  as either positive (\eqn{x_i^{+}}), negative (\eqn{x_i^{-}}), 
#' neutral (\eqn{x_i^{0}}), negator(\eqn{x_i\neg}), or amplifier 
#' (\eqn{x_i^{\uparrow}}).  Neutral words hold no value in the equation but do 
#' affect word count (\eqn{n}).  Each positive (\eqn{x_i^{+}}) and negative 
#' (\eqn{x_i^{-}}) word is then weighted by the amplifiers (\eqn{x_i^{\uparrow}}) 
#' directly proceeding the positive or negative word.  Next, I consider 
#' amplification value, adding the assigned value \eqn{1/n-1} to increase the 
#' polarity relative to sentence length while ensuring that the polarity scores 
#' will remain between the values -1 and 1.  This weighted value for each 
#' polarized word is then multiplied by -1 to the power of the number of negated 
#' (\eqn{x_i\neg}) words directly proceeding the positive or negative word.  
#' Last, these values are then summed and divided by the word count (\eqn{n}) 
#' yielding a polarity score (\eqn{\delta}) between -1 and 1.
#' 
#' \deqn{\delta=\frac{\sum(x_i^{0},\quad x_i^{\uparrow} + 
#' x_i^{+}\cdot(-1)^{\sum(x_i\neg)},\quad x_i^{\uparrow} + 
#' x_i^{-}\cdot(-1)^{\sum(x_i\neg)})}{n}}
#'   
#' Where:
#'     
#' \deqn{x_i^{\uparrow}=\frac{1}{n-1}}
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer 
#' reviews. National Conference on Artificial Intelligence. 
#' 
#' \url{http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
#' @keywords sentiment, polarity
#' @export
#' @examples
#' \dontrun{
#' (poldat <- with(DATA, polarity(state, person)))
#' with(DATA, polarity(state, list(sex, adult)))
#' names(poldat)
#' truncdf(poldat$all, 8)
#' poldat$group
#' poldat2 <- with(mraja1spl, polarity(dialogue, 
#'     list(sex, fam.aff, died)))
#' colsplit2df(poldat2$group)
#' plot(poldat)
#' 
#' poldat3 <- with(rajSPLIT, polarity(dialogue, person))
#' poldat3[["group"]][, "OL"] <- outlier.labeler(poldat3[["group"]][, 
#'     "ave.polarity"])
#' poldat3[["all"]][, "OL"] <- outlier.labeler(poldat3[["all"]][, 
#'     "polarity"])
#' head(poldat3[["group"]], 10)
#' htruncdf(poldat3[["all"]], 15, 8)
#' plot(poldat3)
#' plot(poldat3, nrow=4)
#' }
polarity <-
function (text.var, grouping.var = NULL, positive.list = positive.words, 
    negative.list = negative.words, negation.list = negation.words, 
    amplification.list = increase.amplification.words, 
    rm.incomplete = FALSE, digits = 3, ...) {
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
    allwrds <- c(positive.list, negative.list, negation.list, amplification.list)
    ps <- grepl("\\s", positive.list)
    ns <- grepl("\\s", negative.list)
    ngs <- grepl("\\s", negation.list)
    as <- grepl("\\s", amplification.list)
    spcs <- c(ps, ns, ngs, as)
    if (any(spcs)) {
        dbls <- allwrds[spcs]
        text.var <- mgsub(dbls, gsub("\\s", "~~", dbls), reducer(text.var))
        if (any(ps)) {
            positive.list <- gsub("\\s", "~~", positive.list)
        }
        if (any(ns)) {
            negative.list <- gsub("\\s", "~~", negative.list)
        }
        if (any(ngs)) {
            negation.list <- gsub("\\s", "~~", negation.list)
        }
        if (any(as)) {
            amplification.list <- gsub("\\s", "~~", amplification.list)
        }
    }
    unblank <- function(x) {
        return(x[x != ""])
    }
    if (sum(grepl("a+", tolower(text.var), fixed = TRUE)) > 0) {
        text.var <- mgsub(c("A+", "a+"), "aplus", text.var, 
            fixed = TRUE)
        positive.list <- mgsub(c("A+", "a+"), "aplus", positive.list, 
            fixed = TRUE)
    }
    no.na <- function(x) !is.na(x)
    truer <- function(x) which(!is.na(x) == TRUE)
    SEL <- function(x, y) x[y]
    score <- function(x, y) length(x) - length(y)
    add <- function(x, y, z) x + y + z
    miss <- function(x) ifelse(is.na(x), 0, x)
    NAfill <- which(is.na(text.var))
    negation <- strip(negation.list)
    amplification <- strip(amplification.list)
    x <- as.data.frame(text.var)
    x$words <- unblank(word.split(strip(text.var)))
    x$wc <- word.count(gsub("~~", " ", text.var))
    pos.matchesPOS <- lapply(x$words, function(x) match(x, positive.list))
    neg.matchesPOS <- lapply(x$words, function(x) match(x, negative.list))
    pos <- lapply(pos.matchesPOS, no.na)
    neg <- lapply(neg.matchesPOS, no.na)
    pos1 <- lapply(pos.matchesPOS, truer)
    neg1 <- lapply(neg.matchesPOS, truer)
    x$raw <- unlist(mapply(score, pos1, neg1, SIMPLIFY = FALSE))
    x$pos.words <- mapply(SEL, x$words, pos1, SIMPLIFY = FALSE)
    x$neg.words <- mapply(SEL, x$words, neg1, SIMPLIFY = FALSE)
    L1 <- mapply(function(x, y) x[y - 1], x$words, pos1, SIMPLIFY = FALSE)
    L2 <- mapply(function(x, y) x[y - 1], x$words, neg1, SIMPLIFY = FALSE)
    x$pos.matchesNEG <- lapply(L1, function(x) sum(no.na(match(x, 
        negation))) * (-2))
    x$neg.matchesNEG <- lapply(L2, function(x) sum(no.na(match(x, 
        negation)) * 2))
    x$pos.matchesAMP <- lapply(L1, function(x) no.na(match(x, 
        amplification)))
    x$neg.matchesAMP <- lapply(L2, function(x) no.na(match(x, 
        amplification)))
    ans <- list()
    for (i in 1:nrow(x)) {
        ans[[i]] <- numeric(0)
        for (j in 1:length(x[[i, "neg.matchesAMP"]])) {
            ans[[i]][j] <- ifelse(x$neg.matchesAMP[[i]][j], as.numeric(1/(x[i, 
                "wc"] - 1)), 0)
        }
    }
    AMPneg <- lapply(ans, function(x) sum(miss(x)) * -1)
    ans2 <- list()
    for (i in 1:dim(x)[1]) {
        ans2[[i]] <- numeric(0)
        for (j in 1:length(x[[i, "pos.matchesAMP"]])) {
            ans2[[i]][j] <- ifelse(x$pos.matchesAMP[[i]][j], 
                as.numeric(1/(x[i, "wc"] - 1)), 0)
        }
    }
    AMPpos <- lapply(ans2, function(x) sum(miss(x)))
    x$negation.adj.raw <- mapply(add, x$raw, x$pos.matchesNEG, 
        x$neg.matchesNEG)
    x$amplification.adj.raw <- mapply(add, x$negation.adj.raw, 
        AMPneg, AMPpos)
    x$polarity <- x$amplification.adj.raw/wc(x$words)
    x <- x[, c("text.var", "wc", "polarity", "raw", "negation.adj.raw", 
        "amplification.adj.raw", "pos.words", "neg.words")]
    if (any(is.na(unlist(x)))) {
        x[NAfill, 1:8] <- NA
    }
    DF <- data.frame(group = grouping, x[, c("text.var", 
        "wc", "polarity")])
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    na.instruct <- function(x, y) {
        LIST <- split(x, y)
        z <- unlist(sapply(LIST, function(x) !all(is.na(x[, "text.var"])), 
            USE.NAMES = FALSE))
        return(z)
    }
    L2 <- lapply(split(DF, DF[, "group"]), function(x) {
        if (nrow(x)==1 && is.na(x[, "text.var"])) {
            return(data.frame(group=as.character(x[1, 1]),
                total.sentences=NA, 
                total.words=NA,
                ave.polarity=NA))
        } 
        x <- na.omit(x)
        data.frame(group=as.character(x[1, 1]),
            total.sentences=nrow(x), 
            total.words=sum(x[, "wc"]),
            ave.polarity=mean(x[, "polarity"]))
    })
    DF2 <- data.frame(do.call(rbind, L2), row.names = NULL)
    DF2 <- DF2[order(DF2[, "ave.polarity"]), ]
    x <- data.frame(group=grouping, x, row.names=NULL)
    names(x)[1] <- names(DF2)[1] <- G
    o <- list(all = x, group = DF2, digits = digits)
    class(o) <- "polarity"
    return(o)
}

#' Prints a polarity Object
#' 
#' Prints a polarity object.
#' 
#' @param x The polarity object.
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print polarity
#' @S3method print polarity
print.polarity <-
function(x, digits = NULL, ...) {
    cat("POLARITY BY GROUP\n=================\n")
    WD <- options()[["width"]]
    options(width=3000)
    y <- x$group
    if (is.null(digits)) {
        digits <- x$digits
    }
    y[, "ave.polarity"] <- round(y[, "ave.polarity"], digits = digits)
    print(y)
    options(width=WD)
}

#' Plots a polarity Object
#' 
#' Plots a polarity object as a heat map Gantt plot with polarity over 
#' time (measured in words) and polarity scores per sentence.  In the Gantt 
#' plot the black dots are the average polarity per grouping variable.
#' 
#' @param x The polarity object.
#' @param bar.size The size of the bars used in the Gantt plot.
#' @param low The color to be used for lower values.
#' @param mid The color to be used for mid-range values (default is a less 
#' striking color).
#' @param high The color to be used for higher values.
#' @param ave.polarity.shape The shape of the average polarity score used in the 
#' dot plot.
#' @param alpha Transparency level of points (ranges between 0 and 1).
#' @param shape The shape of the points used in the dot plot.
#' @param point.size The size of the points used in the dot plot.
#' @param jitter Amount of vertical jitter to add to the points.
#' @param nrow The number of rows in the dotplot legend (used when the number of 
#' grouping variables makes the legend too wide).  If NULL no legend if plotted.
#' @param na.rm logical. Should missing values be removed?
#' @param \ldots ignored
#' @return Invisibly returns the \code{ggplot2} objects that form the larger 
#' plot.  
#' @method plot polarity
#' @import ggplot2 gridExtra scales RColorBrewer
#' @S3method plot polarity
plot.polarity <- function(x, bar.size = 5, low = "red", mid = "grey99", 
    high = "blue", ave.polarity.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, ...){
    Polarity <- group <- ave.polarity <- unit <- NULL
    dat <- x[["group"]]
    dat2 <- x[["all"]]
    if (na.rm) {
       dat <- na.omit(dat)
       dat2 <- na.omit(dat2)
    }
    G <- names(dat)[1]
    nms <- c("group", "dialogue", "word_count", "Polarity")
    names(dat)[c(1, 2)] <-  nms[1:2]
    names(dat2)[1:4] <- nms
    dat2 <- data.frame(dat2, with(dat2, 
        gantt(dialogue, list(group, seq_along(group)), plot = FALSE)))
    if (is.null(nrow)) {
        leg <- FALSE
        nrow <- 1
    } else {
        leg <- TRUE
    }
    XX <- ggplot(dat2, aes(color = Polarity )) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (words)") + ylab(gsub("\\&", " & ", G)) +
        scale_colour_gradientn(colours = c(low, mid, high)) +
        theme_bw() + theme(legend.position="bottom") + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75))
    YY <- ggplot(dat2, aes(y=group, x=Polarity, colour = group)) + 
        geom_point(data = dat, aes(x=ave.polarity), shape = ave.polarity.shape, 
            size = 6, show_guide=FALSE) +
        geom_point(alpha = alpha, shape = shape, 
            size = point.size, position = position_jitter(height = jitter)) +
        geom_point(data = dat, aes(x=ave.polarity), shape = 19, 
            size = 1.5, colour = "black", show_guide=FALSE) +
        ylab(gsub("\\&", " & ", G)) +
        scale_color_discrete(name= G) 
    if (leg) {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="bottom")  +
            guides(col = guide_legend(nrow = nrow, byrow = TRUE, 
                override.aes = list(shape = shape, alpha = 1)))
    } else {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="none")       
    } 
    grid.arrange(XX, YY, nrow = 2)
    invisible(list(p1 = XX, p2 = YY))
}
