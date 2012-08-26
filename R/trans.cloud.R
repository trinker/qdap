#' Generate Word Clouds by Grouping Variable
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param word.list %% ~~Describe \code{word.list} here~~
#' @param stem %% ~~Describe \code{stem} here~~
#' @param target.words %% ~~Describe \code{target.words} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
#' @param min.freq %% ~~Describe \code{min.freq} here~~
#' @param caps %% ~~Describe \code{caps} here~~
#' @param caps.list %% ~~Describe \code{caps.list} here~~
#' @param random.order %% ~~Describe \code{random.order} here~~
#' @param rot.per %% ~~Describe \code{rot.per} here~~
#' @param cloud.colors %% ~~Describe \code{cloud.colors} here~~
#' @param cloud.font %% ~~Describe \code{cloud.font} here~~
#' @param title.font %% ~~Describe \code{title.font} here~~
#' @param title.color %% ~~Describe \code{title.color} here~~
#' @param title.padj %% ~~Describe \code{title.padj} here~~
#' @param title.location %% ~~Describe \code{title.location} here~~
#' @param title.cex %% ~~Describe \code{title.cex} here~~
#' @param proportional %% ~~Describe \code{proportional} here~~
#' @param max.word.size %% ~~Describe \code{max.word.size} here~~
#' @param min.word.size %% ~~Describe \code{min.word.size} here~~
#' @param legend %% ~~Describe \code{legend} here~~
#' @param legend.cex %% ~~Describe \code{legend.cex} here~~
#' @param legend.location %% ~~Describe \code{legend.location} here~~
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
trans.cloud <-
function(text.var = NULL, grouping.var = NULL, word.list = NULL, stem = FALSE, 
    target.words = NULL, expand.target = TRUE, target.exclude = NULL,
    stopwords = NULL, min.freq = 1, caps = TRUE, caps.list = NULL, 
    random.order = FALSE, rot.per = 0.0, cloud.colors = NULL, title = TRUE, 
    cloud.font = NULL, title.font = NULL, title.color = "black", 
    title.padj = -4.5, title.location = 3, title.cex = NULL, title.names = NULL,
    proportional = FALSE, max.word.size = NULL, min.word.size = 0.5,
    legend = NULL, legend.cex = .8, legend.location = c(-.03, 1.03), rm.underscore = FALSE, ...) {
    suppressWarnings(require(wordcloud))
    if (!is.null(text.var)){
        word.list <- word_list(text.var = text.var, 
            grouping.var = grouping.var, rm.underscore = rm.underscore)[["cwl"]]
    }
    if(is.list(word.list)) {
        PRO <- max(sapply(word.list, length))
    } else {
        PRO <- length(word.list)
    }
    if(is.null(comment(word.list))){
        word.list <- word.list
    } else {
        if (comment(word.list) %in% "bagOwords"){
            word.list <- word.list
        } else {
            if (comment(word.list) %in% "freqList") {
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
        title.names) {
        if(is.list(target.words) & length(target.words)==1) {
            target.words <- unlist(target.words)
        }
        TWstatus <- is.vector(target.words) & !is.list(target.words)
        if (is.vector(target.words) & !is.list(target.words)) {
            target.words <- list(target.words)
        } 
        if ((length(target.words) + 1) != length(cloud.colors) & 
            !is.null(cloud.colors)) {
                warning("length(cloud.colors) should = length(target.words) + 1")
        }       
        if (stem) {
            require(tm);require(Snowball)
             df <- tm::stemDocument(words)
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
        if(proportional) {
            df2$freq <- floor((PRO/length(words))*df2$freq) 
        }
        COL1 <- if (stem & !is.null(target.words)) {
            sapply(target.words, tm::stemDocument)
        } else {
            if (!stem & !is.null(target.words)) {
                target.words
            } else {
                NULL
            }
        }       
        COL1 <- if(!is.null(target.words)){ 
            capitalize <- function(x) {
                simpleCap <- function(x) {
                    s <- strsplit(x, " ")[[1]]
                    paste(toupper(substring(s, 1,1)), substring(s, 2),
                        sep="", collapse=" ")
                }
                unlist(lapply(x, simpleCap)) 
            }
            FUN <- function(x) c(tolower(x), capitalize(x))
            sapply(COL1, FUN)
        } else {
            NULL
        }
        COL <- if (is.null(cloud.colors)) {
            rep("black", length(df2$word))
        } else {
            ncc <- length(cloud.colors)
           if (TWstatus) {
                text2color(words = df2$word, recode.words = list(c(COL1)), 
                    colors = cloud.colors)
            } else {
                text2color(words = df2$word, recode.words = COL1, 
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
        if (dev.interactive()) {
            dev.new()
        }
        wordcloud::wordcloud(df2[, 1], df2[, 2], colors = COL, rot.per = rot.per, 
            min.freq = min.freq, ordered.colors = TRUE, vfont = font, 
            random.order = random.order, scale = c(Scale, word.size))
        if (title) {
            mtext(text, side = side, padj = title.padj, col = title.color,
                family = title.font, cex = title.cex)
        }
        if (!is.null(legend)){
            par(mar = rep(0, 4), xpd = NA)
            legend(x = legend.location[1], y = legend.location[2], 
                cex = legend.cex, legend = legend, 
                fill = cloud.colors[1:length(legend)])
            par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
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
        target.words <- lapply(TF, function(i) uni[i])
    }
    if (!is.null(target.exclude)) {
        target.words <- lapply(target.words, function(x) x[!x %in% target.exclude])
    }
    lapply(seq_along(word.list), function(i) CLOUD(words = word.list[[i]], 
        stem = stem, target.words = target.words, stopwords = stopwords,
        proportional = proportional, PRO = PRO, word.size2 = max.word.size, 
        min.freq = min.freq, word.size = min.word.size, random.order = random.order, 
        cloud.colors = cloud.colors, caps = caps, caps.list = caps.list, 
        font = cloud.font, title.font = title.font, title.cex = title.cex,
        title.color = title.color, side = title.location, legend = legend, 
        legend.cex = legend.cex, legend.location = legend.location, 
        text = namers[i], ...)
    )
}
# trans.cloud <-
# function(text.var = NULL, grouping.var = NULL, word.list = NULL, stem = FALSE, 
#     target.words = NULL, expand.target = TRUE,
#     stopwords = NULL, min.freq = 1, caps = TRUE, caps.list = NULL, 
#     random.order = FALSE, rot.per = 0.0, cloud.colors = NULL, 
#     cloud.font = NULL, title.font = NULL, title.color = NULL, 
#     title.padj = -4.5, title.location = 3, title.cex = NULL, title.names = NULL,
#     proportional = FALSE, max.word.size = NULL, min.word.size = 0.5,
#     legend = NULL, legend.cex = .8, legend.location = c(-.03, 1.03), ...) {
#     suppressWarnings(require(wordcloud))
#     if (!is.null(text.var)){
#         word.list <- qda(text.var = text.var, 
#             grouping.var = grouping.var)[["cwl"]]
#     }
#     if(length(word.list)>1) {
#         PRO <- max(sapply(word.list, length))
#     } else {
#         PRO <- length(word.list)
#     }
#     if(is.null(comment(word.list))){
#         word.list <- word.list
#     } else {
#         if (comment(word.list) %in% "bagOwords"){
#             word.list <- word.list
#         } else {
#             if (comment(word.list) %in% "freqList") {
#                 word.list <- freqTab2words(word.list)
#             } else {
#                 word.list <- lapply(word.list, qda.handler)
#             }
#         }
#     }
#     CLOUD <- function(words, stem, target.words, stopwords, min.freq, 
#         word.size, word.size2, random.order, cloud.colors, caps, 
#         caps.list, title.color, text, side, PRO, proportional, font, 
#         title.font, title.cex, legend, legend.cex, legend.location,
#         title.names) {
#         if(is.list(target.words) & length(target.words)==1) {
#           target.words <-unlist(target.words)
#         }
#         TWstatus <- is.vector(target.words) & !is.list(target.words)
#         if (is.vector(target.words) & !is.list(target.words)) {
#             target.words <- list(target.words)
#         } 
#         if ((length(target.words) + 1) != length(cloud.colors) & 
#             !is.null(cloud.colors)) {
#             stop("length(cloud.colors) should = length(target.words) + 1")
#         }       
#         if (stem) {
#             require(tm);require(Snowball)
#              df <- tm::stemDocument(words)
#         }  else {
#             df <- words
#         }
#         if (!is.null(stopwords)) {
#             df <- df[!df %in% stopwords] 
#         }
#         if (caps) {
#             df <- capitalizer(df, caps.list) 
#         }
#         df2 <- as.data.frame(table(df), stringsAsFactors = FALSE)
#         names(df2) <- c("word", "freq")
#         if(proportional) {
#             df2$freq <- floor((PRO/length(words))*df2$freq) 
#         }
#         COL1 <- if (stem & !is.null(target.words)) {
#             sapply(target.words, tm::stemDocument)
#         } else {
#             if (!stem & !is.null(target.words)) {
#                 target.words
#             } else {
#                 NULL
#             }
#         }       
#         COL1 <- if(!is.null(target.words)){ 
#             capitalize <- function(x) {
#                 simpleCap <- function(x) {
#                     s <- strsplit(x, " ")[[1]]
#                     paste(toupper(substring(s, 1,1)), substring(s, 2),
#                         sep="", collapse=" ")
#                 }
#                 unlist(lapply(x, simpleCap)) 
#             }
#             FUN <- function(x) c(tolower(x), capitalize(x))
#             sapply(COL1, FUN)
#         } else {
#             NULL
#         }
#         COL <- if (is.null(cloud.colors)) {
#             rep("black", length(df2$word))
#         } else {
#             ncc <- length(cloud.colors)
#            if (TWstatus) {
#                 text2color(words = df2$word, recode.words = list(c(COL1)), 
#                     colors = cloud.colors)
#             } else {
#                 text2color(words = df2$word, recode.words = COL1, 
#                     colors = cloud.colors)
#             }
#         }
#         Scale <- if(!is.null(word.size2)) {
#               word.size2   
#         } else {
#            if (is.null(word.size2) & proportional) {  
#                 3
#             } else {       
#                 mean(df2[, 2] + 1)
#             }
#         }
#         if (dev.interactive()) 
#             dev.new()
#         wordcloud::wordcloud(df2[, 1], df2[, 2], colors = COL, rot.per = rot.per, 
#             min.freq = min.freq, ordered.colors = TRUE, vfont = font, 
#             random.order = random.order, scale = c(Scale, word.size))
#         if (!is.null(title.color)) {
#             mtext(text, side = side, padj = title.padj, col = title.color,
#                 family = title.font, cex = title.cex)
#         }
#         if (!is.null(legend)){
#             par(mar = rep(0, 4), xpd = NA)
#             legend(x = legend.location[1], y = legend.location[2], 
#                 cex = legend.cex, legend = legend, 
#                 fill = cloud.colors[1:length(legend)])
#             par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
#         }
#     }  #end of CLOUD function
#     if (!is.list(word.list)) {
#         word.list <- list(word.list) 
#     } 
#     if(!is.null(title.names)){
#         namers <- title.names
#     } else { 
#         namers <- names(word.list)
#     }
#     if (expand.target) {
#         uni <- unique(unlist(word.list))
#         target.words <- lapply(term.find(uni, target.words), function(i) uni[i])
#     }
#     lapply(seq_along(word.list), function(i) CLOUD(words = word.list[[i]], 
#         stem = stem, target.words = target.words, stopwords = stopwords,
#         proportional = proportional, PRO = PRO, word.size2 = max.word.size, 
#         min.freq = min.freq, word.size = min.word.size, random.order = random.order, 
#         cloud.colors = cloud.colors, caps = caps, caps.list = caps.list, 
#         font = cloud.font, title.font = title.font, title.cex = title.cex,
#         title.color = title.color, side = title.location, legend = legend, 
#         legend.cex = legend.cex, legend.location = legend.location, 
#         text = namers[i], ...)
#     )
# }
# 
# 
