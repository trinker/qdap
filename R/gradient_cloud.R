#' Gradient Word Cloud
#' 
#' Produces a gradient word cloud colored by a binary grouping variable.
#' 
#' @param text.var The text variable.
#' @param bigroup.var A binary grouping variable.
#' @param rev.binary logical.  If \code{TRUE} the ordering of the binary levels of 
#' bigroup.var is reversed.
#' @param X The first gradient color for variable X.
#' @param Y The second gradient color for variable Y.
#' @param stem logical.  If \code{TRUE} the \code{text.var} will be stemmed.
#' @param stopwords Words to exclude from the cloud.  Words will be removed 
#' after determining proportional word usage.
#' @param caps logical.  If \code{TRUE} selected words will be capitalized.
#' @param caps.list A vector of words to capitalize (\code{caps} must be 
#' \code{TRUE}).
#' @param I.list logical.  If \code{TRUE} capitalizes I words and contractions.
#' @param random.order Plot words in random order. If \code{FALSE}, they will be 
#' plotted in decreasing frequency.
#' @param rot.per Proportion words with 90 degree rotation.
#' @param min.freq An integer value indicating the minimum frequency a word must 
#' appear to be included.
#' @param max.word.size A size argument to control the minimum size of the words.
#' @param min.word.size A size argument to control the maximum size of the words.
#' @param breaks An integer describing the number of breaks (odd numbers will be 
#' rounded up).
#' @param cloud.font The font family of the cloud text.
#' @param title A character string used as the plot title.
#' @param title.font The font family of the cloud title. 
#' @param title.color A character vector of length one corresponding to the 
#' color of the title.
#' @param title.padj Adjustment for the title. For strings parallel to the axes, 
#' padj = 0 means right or top alignment, and padj = 1 means left or bottom 
#' alignment.
#' @param title.location On which side of the plot (1=bottom, 2=left, 3=top, 
#' 4=right).
#' @param title.cex Character expansion factor for the title. \code{NULL} and 
#' \code{NA} are equivalent to 1.0. 
#' @param legend.cex Character expansion factor for the legend. \code{NULL} and 
#' \code{NA} are equivalent to 1.0. 
#' @param legend.location A vector of length 4 denoting the lower left (x and y 
#' left) and upper right (x and y right) coordinates of the rectangle of colors 
#' in user coordinates.
#' @param char2space A vector of characters to be turned into spaces.  
#' @return Plots a gradient word cloud and invisibly returns the dataframe used 
#' to make the cloud.
#' @details Breaking is done using \code{\link[stats]{quantile}}.  This will 
#' ensure a certain percentage of words will be colored at each bin.
#' @seealso \code{\link[qdap]{trans_cloud}},
#' \code{\link[wordcloud]{wordcloud}},
#' \code{\link[plotrix]{color.legend}}
#' @keywords heatcloud
#' @export
#' @importFrom qdapTools lookup
#' @importFrom plotrix color.legend
#' @importFrom wordcloud wordcloud
#' @examples
#' \dontrun{
#' DATA$state <- space_fill(DATA$state, c("is fun", "too fun", "you liar"))
#' 
#' gradient_cloud(DATA$state, DATA$sex, title="fun")
#' gradient_cloud(DATA$state, DATA$sex, title="fun", rev.binary = TRUE)
#' gradient_cloud(DATA$state, DATA$sex, title="fun", max.word.size = 5,
#'     min.word.size = .025)
#'     
#' with(mraja1, gradient_cloud(dialogue, died, stopwords = Top25Words, 
#'     rot.per = .5, title="Heatcloud", title.color="orange", title.cex=1.75))    
#' x <- with(subset(mraja1, fam.aff %in% qcv(cap, mont)), 
#'     gradient_cloud(dialogue, fam.aff))
#' head(x) 
#' 
#' ## 2012 U.S. Presidential Debates
#' invisible(lapply(split(pres_debates2012, pres_debates2012$time), function(x) {
#'     x <- x[x$person %in% qcv(ROMNEY, OBAMA), ]
#'     dev.new()
#'     gradient_cloud(x$dialogue, x$person, 
#'         title = paste("Debate", char2end(x$time[1])),
#'         stopwords = BuckleySaltonSWL,
#'         X = "blue", Y = "red", 
#'         max.word.size = 2.2, 
#'         min.word.size = 0.55
#'     )
#' }))
#'         
#' }
gradient_cloud <- function(text.var, bigroup.var, rev.binary = FALSE, X = "red", 
    Y = "blue", stem = FALSE, stopwords = NULL, caps = TRUE, caps.list = NULL, 
    I.list = TRUE, random.order = FALSE, rot.per = 0.0, min.freq = 1, 
    max.word.size = NULL, min.word.size = 0.5, breaks = 10, cloud.font = NULL, 
    title = NULL, title.font = NULL, title.color = "black", title.padj = .25, 
    title.location = 3, title.cex = NULL, legend.cex = .8, 
    legend.location = c(.025, .025, .25, .04), char2space = "~~") {
    text.var <- as.character(text.var)
    bigroup.var <- drop.levels(bigroup.var)
    if (length(unique(bigroup.var)) != 2) {
        stop("bigroup.var must contain exactly 2 levels")
    }
    if (rev.binary) {
        bigroup.var <- factor(bigroup.var, levels = rev(levels(bigroup.var)))
    }
    if (stem) {
        text.var <- stemmer(text.var)
    }
    word.freq <- wfdf(text.var, bigroup.var)
    nms <- colnames(word.freq)[-1]
    colnames(word.freq)[-1] <- utils::tail(LETTERS, 2)
    wf2 <- word.freq[, -1]/colSums(word.freq[, -1])
    colnames(wf2) <- paste0("prop_", colnames(wf2))
    WF <- data.frame(word.freq, wf2, check.names = FALSE)
    WF[, "total"] <- rowSums(word.freq[, -1])
    WF[, "diff"] <- wf2[, 1] - wf2[, 2]
    low <- WF[, "diff"][WF[, "diff"] < 0]
    high <- WF[, "diff"][WF[, "diff"] > 0]
    lcuts <- stats::quantile(low, seq(0, 1, length.out = round(breaks/2)))
    hcuts <- stats::quantile(high, seq(0, 1, length.out = round(breaks/2)))
    cts <- as.numeric(unique(sort(c(-1, lcuts, 0, hcuts, 1))))
    WF[, "trans"] <- cut(WF[, "diff"], breaks=cts)
    if (!is.null(stopwords)) {
        WF <- WF[!WF[, 1] %in% tolower(stopwords),]
    }
    if (caps) {
        WF[, 1] <- capitalizer(WF[, 1], caps.list = caps.list, 
            I.list = I.list, apostrophe.remove = TRUE)
    }
    if(is.null(max.word.size )) {
        max.word.size  <- mean(WF[, "total"] + 1)
    }
    colfunc <- grDevices::colorRampPalette(c(X, Y))
    WF[, "colors"] <- lookup(WF[, "trans"], levels(WF[, "trans"]),
        rev(colfunc(length(levels(WF[, "trans"])))))
    OP <- graphics::par()[["mar"]]
    on.exit(graphics::par(mar = OP))
    graphics::par(mar=c(7,1,1,1))
    wordcloud(WF[, 1], WF[, "total"], min.freq = min.freq, 
        colors = WF[, "colors"], rot.per = rot.per, random.order = random.order, 
        ordered.colors = TRUE, vfont = cloud.font, 
        scale = c(max.word.size , min.word.size))
    if (!is.null(title)) {
        graphics::mtext(title, side = title.location, padj = title.padj, 
            col = title.color, family = title.font, cex = title.cex)
    }
    COLS <- colfunc(length(levels(WF[, "trans"])))
    LL <- legend.location
    color.legend(LL[1], LL[2], LL[3], LL[4], nms, COLS, cex = legend.cex)
    colnames(WF)[1:5] <- c("words", nms, paste0("prop.", nms))
    WF  <- WF[, c(1:3, 6, 4:5, 7:9)]
    return(invisible(WF))
}


drop.levels <- function(x, reorder=TRUE, ...){
  x <- x[, drop=TRUE]
  if(reorder) x <- reorder(x, ...)
  x
}

