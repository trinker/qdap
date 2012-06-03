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
trans.cloud <-
function(word.list, stem = FALSE, target.words = NULL, 
    stopwords = NULL, min.freq = 1, caps = TRUE, caps.list = NULL, 
    random.order = FALSE, rot.per = 0.5, cloud.colors = NULL, 
    cloud.font = NULL, title.font = NULL, title.color = NULL, 
    title.padj = -4.5, title.location = 3, title.cex = NULL,
    proportional = FALSE, max.word.size = NULL, min.word.size = 0.5,
    legend = NULL, legend.cex = .8, legend.location = c(-.03, 1.03)) {
    suppressWarnings(require(wordcloud))
    PRO <- if(length(word.list)>1) {
            max(sapply(word.list, length))
        } else {
            length(word.list)
        }
    word.list <- if(is.null(comment(word.list))){
        word.list
    } else {
        if (comment(word.list) %in% "bagOwords"){
            word.list
        } else {
            if (comment(word.list) %in% "freqList") {
                freqTab2words(word.list)
            } else {
                lapply(word.list, qda.handler)
            }
        }
    }
    CLOUD <- function(words, stem, target.words, stopwords, min.freq, 
        word.size, word.size2, random.order, cloud.colors, caps, 
        caps.list, title.color, text, side, PRO, proportional, font, 
        title.font, title.cex, legend, legend.cex, legend.location) {
        target.words <- if(is.list(target.words) & length(target.words)==1) {
            unlist(target.words)
        } else {
            target.words
        }
        TWstatus <- is.vector(target.words) & !is.list(target.words)
        target.words <- if (is.vector(target.words) & !is.list(target.words)) {
            list(target.words)
        } else {
            target.words
        } 
        if ((length(target.words) + 1) != length(cloud.colors) & 
            !is.null(cloud.colors)) {
            stop("length(cloud.colors) should = length(target.words) + 1")
        }       
        df <- if (stem) {
            require(tm);require(Snowball)
            tm::stemDocument(words)
        } else {
            words
        }
        df <- if (!is.null(stopwords)) df[!df %in% stopwords] else df
        df <- if (caps) capitalizer(df, caps.list) else df
        df2 <- as.data.frame(table(df), stringsAsFactors = FALSE)
        names(df2) <- c("word", "freq")
        df2$freq <- if(proportional) floor((PRO/length(words))*df2$freq) else df2$freq

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
        text2color <- function(text, words, colors, nomatch) {
            lookup <- lapply(seq_along(words), function(n) cbind(words[[n]], 
                colors[n]))
            lookup <- do.call("rbind.data.frame", lookup)
            lookup <- apply(lookup, 2, as.character)
            recode <- lookup[match(text, lookup[, 1]), 2]
            recode[is.na(recode)] <- nomatch
            return(recode)
        }
        COL <- if (is.null(cloud.colors)) {
            rep("black", length(df2$word))
        } else {
            ncc <- length(cloud.colors)
           if (TWstatus) {
                text2color(text = df2$word, words = list(c(COL1)), colors = cloud.colors[-ncc], 
                    nomatch = cloud.colors[ncc])
            } else {
                text2color(text = df2$word, words = COL1, colors = cloud.colors[-ncc], 
                    nomatch = cloud.colors[ncc])
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
        if (dev.interactive()) 
            dev.new()
        wordcloud::wordcloud(df2[, 1], df2[, 2], colors = COL, rot.per = rot.per, 
            min.freq = min.freq, ordered.colors = TRUE, vfont = font, 
            random.order = random.order, scale = c(Scale, word.size))
        if (!is.null(title.color)) {
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
    word.list <- if (!is.list(word.list)) list(word.list) else word.list
    lapply(seq_along(word.list), function(i) CLOUD(words = word.list[[i]], 
        stem = stem, target.words = target.words, stopwords = stopwords,
        proportional = proportional, PRO = PRO, word.size2 = max.word.size, 
        min.freq = min.freq, word.size = min.word.size, random.order = random.order, 
        cloud.colors = cloud.colors, caps = caps, caps.list = caps.list, 
        font = cloud.font, title.font = title.font, title.cex = title.cex,
        title.color = title.color, side = title.location, legend = legend, 
        legend.cex = legend.cex, legend.location = legend.location, 
        text = names(word.list)[i]))
}
