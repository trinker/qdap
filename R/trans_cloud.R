#' Word Clouds by Grouping Variable
#' 
#' Produces word clouds with optional theme coloring by grouping variable.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.  
#' @param word.list A frequency word list passed from 
#' \code{\link[qdap]{word_list}}.
#' @param stem logical.  If \code{TRUE} the \code{text.var} will be stemmed.
#' @param target.words A named list of vectors of words whose length corresponds 
#' to \code{cloud.colors} (+1 length in cloud colors for non-matched terms).
#' @param expand.target logical.  If \code{TRUE} \code{\link[base]{agrep}}  
#' will be used to expand the \code{target.words}.
#' @param target.exclude A vector of words to exclude from the 
#' \code{target.words}.
#' @param stopwords Words to exclude from the cloud.
#' @param min.freq An integer value indicating the minimum frequency a word must 
#' appear to be included.
#' @param caps logical.  If \code{TRUE} selected words will be capitalized.
#' @param caps.list A vector of words to capitalize (\code{caps} must be 
#' \code{TRUE}).
#' @param random.order Plot words in random order. If false, they will be 
#' plotted in decreasing frequency.
#' @param rot.per Proportion words with 90 degree rotation.
#' @param cloud.colors A vector of colors equal to the length of target words +1.
#' @param title logical.  If \code{TRUE} adds a title corresponding to the 
#' \code{grouping.var}.
#' @param cloud.font The font family of the cloud text.
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
#' @param title.names Optional vector of title names equal in length to the 
#' grouping.var that will override the default use of the grouping.var names.  
#' @param proportional logical.  If \code{TRUE} scales the word clouds across 
#' grouping.var to allow cloud to cloud comparisons.
#' @param max.word.size A size argument to control the minimum size of the words.
#' @param min.word.size A size argument to control the maximum size of the words.
#' @param legend A character vector of names corresponding to the number of 
#' vectors in target.words.
#' @param legend.cex Character expansion factor for the legend. \code{NULL} and 
#' \code{NA} are equivalent to 1.0. 
#' @param legend.location The x and y co-ordinates to be used to position the 
#' legend.
#' @param char.keep A character vector of symbol character (i.e., punctuation) 
#' that strip should keep.  The default is to strip everything except 
#' apostrophes.  This enables the use of special characters to be turned into 
#' spaces or for characters to be retained.
#' @param char2space A vector of characters to be turned into spaces.  If 
#' \code{char.keep} is \code{NULL}, \code{char2space} will activate this 
#' argument.
#' @return Returns a series of word cloud plots with target words (themes) 
#' colored.
#' @seealso \code{\link[wordcloud]{wordcloud}},
#' \code{\link[qdap]{gradient_cloud}}
#' @keywords wordcloud
#' @export
#' @importFrom qdapTools text2color
#' @importFrom wordcloud wordcloud
#' @examples
#' \dontrun{
#' terms <- list(
#'     I=c("i", "i'm"),
#'     mal=qcv(stinks, dumb, distrust),
#'     articles=qcv(the, a, an),
#'     pronoun=qcv(we, you)
#' )
#' 
#' with(DATA, trans_cloud(state, person, target.words=terms, 
#'     cloud.colors=qcv(red, green, blue, black, gray65), 
#'     expand.target=FALSE, proportional=TRUE, legend=c(names(terms), 
#'     "other")))
#' 
#' with(DATA, trans_cloud(state, person, target.words=terms,
#'     stopwords=exclude(with(DATA, unique(bag_o_words(state))), 
#'         unique(unlist(terms))), 
#'     cloud.colors=qcv(red, green, blue, black, gray65), 
#'     expand.target=FALSE, proportional=TRUE, legend=names(terms)))
#'     
## with(mraja1, trans_cloud(dialogue, person,
##     target.words=list(
##         positive=qdapDictionaries::positive.words, 
##         negative=qdapDictionaries::negative.words,
##         negator=qdapDictionaries::negation.words, 
##         amplifier=qdapDictionaries::amplification.words,
##         deamplifier=qdapDictionaries::deamplification.words),
##     cloud.colors=qcv(green, red2, black, orange, turquoise3, gray85),
##     expand.target=FALSE, proportional=TRUE, legend=names(terms)))
#'     
#' #color the negated phrases opposite:
#' DATA <- qdap::DATA
#' DATA[1, 4] <- "This is not good!"
#' DATA[8, 4] <- "I don't distrust you."
#' 
#' DATA$state <- space_fill(DATA$state, paste0(negation.words, " "), 
#'     rm.extra = FALSE)
#' 
#' txt <- gsub("~~", " ", breaker(DATA$state))
#' rev.neg <- sapply(negation.words, paste, negative.words)
#' rev.pos <- sapply(negation.words, paste, positive.words)
#' 
#' 
#' tw <- list(
#'     positive=c(positive.words, rev.neg[rev.neg %in% txt]), 
#'     negative=c(negative.words, rev.pos[rev.pos %in% txt])
#' )
#' 
#' 
#' with(DATA, trans_cloud(state, person,
#'     target.words=tw,
#'     cloud.colors=qcv(darkgreen, red, gray65),
#'     expand.target=FALSE, proportional=TRUE, legend=names(tw)))
#'
#' DATA <- qdap::DATA  ## Reset DATA
#' }
trans_cloud <-
function(text.var = NULL, grouping.var = NULL, word.list = NULL, stem = FALSE, 
    target.words = NULL, expand.target = TRUE, target.exclude = NULL,
    stopwords = NULL, min.freq = 1, caps = TRUE, caps.list = NULL, 
    random.order = FALSE, rot.per = 0.0, cloud.colors = NULL, title = TRUE, 
    cloud.font = NULL, title.font = NULL, title.color = "black", 
    title.padj = -4.5, title.location = 3, title.cex = NULL, title.names = NULL,
    proportional = FALSE, max.word.size = NULL, min.word.size = 0.5,
    legend = NULL, legend.cex = .8, legend.location = c(-.03, 1.03), 
    char.keep = "~~", char2space = "~~") {
    if(!is.null(char2space) & is.null(char.keep)) {
        char.keep <- char2space
    }
    if (!is.null(text.var)){
        word.list <- word_list(text.var = text.var, 
            grouping.var = grouping.var, char.keep = char.keep)[["swl"]]
    }
    if(is.list(word.list)) {
        PRO <- max(sapply(word.list, length))
    } else {
        PRO <- length(word.list)
    }
    if(methods::is(word.list, "word_list")){
        word.list <- word.list
    } else {
        if (methods::is(word.list, "bagOwords")){
            word.list <- word.list
        } else {
            if (methods::is(word.list, "freqList")) {
                word.list <- freqTab2words(word.list)
            } else {
                word.list <- lapply(word.list, qda.handler)
            }
        }
    }
    CLOUD <- function(words, stem, target.words, stopwords, min.freq, 
        word.size, word.size2, random.order, cloud.colors, caps, 
        caps.list, title.color, text, side, PRO, proportional, font, 
        title.font, title.cex, legend, legend.cex, legend.location,
        title.names, char2space) {
        if(is.list(target.words) & length(target.words)==1) {
            target.words <- unlist(target.words)
        }
        if (is.vector(target.words) & !is.list(target.words)) {
            target.words <- list(target.words)
        } 
        if ((length(target.words) + 1) != length(cloud.colors) & 
            !is.null(cloud.colors)) {
                warning("length(cloud.colors) should = length(target.words) + 1")
        }  
        TWstatus <- is.vector(target.words) & !is.list(target.words)     
        if (stem) {
             df <- stemDocument(words)
        }  else {
            df <- words
        }
        if (!is.null(stopwords)) {
            df <- df[!df %in% stopwords] 
        }
        if (caps) {
            df <- capitalizer(df, caps.list) 
        }
        df2 <- as.data.frame(table(df), stringsAsFactors = FALSE)
        names(df2) <- c("word", "freq")
        if (!is.null(char2space)) {
            df2[, "word"] <- mgsub(pattern = char2space, replacement = " ", 
                text.var = df2[, "word"])
        }
        if(proportional) {
            df2$freq <- floor((PRO/length(words))*df2$freq) 
        }
        if (stem & !is.null(target.words)) {
            COL1 <- sapply(target.words, stemDocument)
        } else {
            if (!stem & !is.null(target.words)) {
                COL1 <- target.words
            } else {
                COL1 <- NULL
            }
        }   
        if (!is.null(char2space)) {
            COL1 <- lapply(COL1, function(x) gsub(char2space, " ", x))
        }   
        if(!is.null(target.words)){ 
            capitalize <- function(x) {
                simpleCap <- function(x) {
                    s <- strsplit(x, " ")[[1]]
                    paste(toupper(substring(s, 1,1)), substring(s, 2),
                        sep="", collapse=" ")
                }
                unlist(lapply(x, simpleCap)) 
            }
            FUN <- function(x) c(tolower(x), capitalize(x))
            COL1 <- lapply(COL1, FUN)
        } else {
            COL1 <- NULL
        }
        if (is.null(cloud.colors)) {
            COL <- rep("black", length(df2$word))
        } else {
            ncc <- length(cloud.colors)
            if (TWstatus) {
                COL <- text2color(words = df2$word, recode.words = list(c(COL1)), 
                    colors = cloud.colors)
            } else {
                COL <- text2color(words = df2$word, recode.words = COL1, 
                    colors = cloud.colors)
            }
        }
        Scale <- if(!is.null(word.size2)) {
              word.size2   
        } else {
           if (is.null(word.size2) & proportional) {  
                3
            } else {       
                mean(df2[, 2] + 1)
            }
        }
        if (grDevices::dev.interactive()) {
            grDevices::dev.new()
        }
        wordcloud(df2[, 1], df2[, 2], colors = COL, rot.per = rot.per, 
            min.freq = min.freq, ordered.colors = TRUE, vfont = font, 
            random.order = random.order, scale = c(Scale, word.size))
        if (title) {
            graphics::mtext(text, side = side, padj = title.padj, col = title.color,
                family = title.font, cex = title.cex)
        }
        if (!is.null(legend)){
            graphics::par(mar = rep(0, 4), xpd = NA)
            legend(x = legend.location[1], y = legend.location[2], 
                cex = legend.cex, legend = legend, 
                fill = cloud.colors[1:length(legend)])
            graphics::par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
        }
    }  #end of CLOUD function
    if (!is.list(word.list)) {
        word.list <- list(word.list) 
    } 
    if(!is.null(title.names)){
        namers <- title.names
    } else { 
        namers <- names(word.list)
    }
    if (expand.target) {
        uni <- unique(unlist(word.list))
        target.words <- lapply(target.words, function(x) x[!is.na(x)])
        TF <- lapply(target.words, function(x){
                if (length(x) == 1) {
                    if (is.na(x) | Trim(x)=="") {
                        FALSE
                    } else {
                        term.find(uni, x)
                    }
                } else {
                    term.find(uni, x)
                }
            }
        )
        target.words <- lapply(TF, function(i) {
            uni[unlist(i)]
        })
    }
    if (!is.null(target.exclude)) {
        target.words <- lapply(target.words, function(x) x[!x %in% target.exclude])
    }
    invisible(lapply(seq_along(word.list), function(i) CLOUD(words = word.list[[i]], 
        stem = stem, target.words = target.words, stopwords = stopwords,
        proportional = proportional, PRO = PRO, word.size2 = max.word.size, 
        min.freq = min.freq, word.size = min.word.size, random.order = random.order, 
        cloud.colors = cloud.colors, caps = caps, caps.list = caps.list, 
        font = cloud.font, title.font = title.font, title.cex = title.cex,
        title.color = title.color, side = title.location, legend = legend, 
        legend.cex = legend.cex, legend.location = legend.location, 
        text = namers[i], char2space = char2space))
    )
}
