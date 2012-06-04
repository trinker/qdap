word.associate <-
function(dataframe, text.var, word, stem = FALSE, 
    TOT = TRUE, stopwords = NULL, wordcloud = TRUE, min.freq = 1, 
    word.size = 1.2, rot.per = 0, text.size = 1.5, random.order = FALSE, 
    list.length = NULL, cloud.colors = NULL) {
    finder <- function(text, word, stem) {
        FINDER <- function(TEXT, word) {
            x <- words(strip(TEXT))
            any(x %in% word)
        }
        if (stem) {
            suppressWarnings(require(tm))
            ST <- stemDocument(word)
            ST2 <- unlist(lapply(text, function(x) paste(
                stemDocument(words(strip(x))), collapse = " ")))
            unlist(lapply(ST2, function(x) FINDER(x, word = ST)))
        } else {
            unlist(lapply(text, function(x) FINDER(x, word = word)))
        }
    } 
    DF <- dataframe
    DF$TOT <- TOT(DF$tot)
    y <- which(finder(text = DF[, text.var], word = word, stem = stem))
    OC <- if (TOT) {
        DF[DF$TOT %in% DF[y, "TOT"], ]
    } else {
        DF[y, ]
    }
    if (wordcloud) {
        require(wordcloud)
        OCtext <- words(strip(as.character(OC[, text.var])))
        OCDF <- data.frame(table(OCtext))
        windows(12, 9)
        x <- matrix(c(1, 2, 2), 1, 3, byrow = TRUE)
        layout(x)
        require(gplots)
        OCDF2 <- OCDF[order(-OCDF$Freq), ]
        names(OCDF2) <- c("word", "freq")
        OCDF3 <- OCDF2
        rownames(OCDF3) <- 1:nrow(OCDF3)
        OCDF2 <- OCDF2[OCDF2$freq > (min.freq - 1), ]
        rownames(OCDF2) <- 1:nrow(OCDF2)
        LL <- if (is.null(list.length)) 
            nrow(OCDF2) else list.length
        textplot(OCDF2[1:LL, ], cex = text.size, halign = "left", valign = "center", 
            mar = c(0, 0, 0, 0))
        scale <- mean(OCDF2$freq + 1)
        COL1 <- if (stem) stemDocument(word) else word
        COL2 <- if(is.null(cloud.colors)) c("red", "black") else cloud.colors
        COL <- ifelse(as.character(OCDF2[, "word"]) %in% COL1, COL2[1], COL2[2])
        wordcloud(as.character(OCDF2[, "word"]), OCDF2[, "freq"], colors = COL, rot.per = rot.per, 
            min.freq = 1, ordered.colors = TRUE, random.order = random.order, scale = c(scale, 
                word.size))    
    }
    return(list(DATAFRAME = OC, FREQ.TABLE = OCDF3))
}
