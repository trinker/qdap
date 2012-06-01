frey <-
function(text.var, grouping.var = NULL, labels = "automatic") {
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
                             if(any(is.na(x))){NA}else{paste(x, collapse = ".")
                                 }
                             }
                         )
                     } else {
                        unlist(grouping.var)
                     } 
                 } 
    text <- as.character(text.var)
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    DF$word.count <- word.count(DF$text.var, missing = 0)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$read.gr <- unlist(by(DF$word.count, DF$group, g))
    LIST <- names(which(tapply(DF$word.count, DF$group, function(x) {
        max(cumsum(x))
    }
        ) >= 300)
    )
    DF2 <- subset(DF, group %in% LIST & read.gr != "NA")
    DF2$group <- DF2$group[, drop = TRUE]
    DF2$sub <- with(DF2, paste(group, read.gr, sep = "_"))
    DF2b <- DF2[order(DF2$sub), ]
    DF2b$cumsum.gr <- unlist(tapply(DF2$word.count, DF2$sub, 
        cumsum))
    DF2 <- DF2b[order(DF2b$group, DF2b$tot.n.sent), ]
    DF2$wo.last <- DF2$cumsum.gr - DF2$word.count
    DF2$hun.word <- 100 - DF2$wo.last
    DF2$frac.sent <- ifelse(DF2$hun.word/DF2$word.count > 1, 
        1, round(DF2$hun.word/DF2$word.count, digits = 3))   
    DF3b <- unique(data.frame(sub = DF2$sub, group = DF2$group))
    DF3 <- data.frame(sub = DF2$sub, group = DF2$group, 
        text.var = DF2$text.var, frac.sent = DF2$frac.sent)
    CHOICES <- tapply(as.character(DF3b$sub), DF3b$group, function(x) {
            sample(x, 3, replace = FALSE)
        }
    )
    CHOICES <- as.character(unlist(CHOICES))
    DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
    DF4$sub <- DF4$sub[, drop = TRUE]
    FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
    DF5 <- aggregate(text.var ~ sub + group, DF4, FUN)
    sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, 
        sum))
    names(sent.per.100) <- "x"
    DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"] 
    hun.grab <- function(x) paste(unblanker(unlist(word.split(
        reducer(unlist(strip(x))))))[1:100], collapse = " ")
    DF5$syll.count <- syllable.sum(lapply(DF5$text.var, hun.grab))
    DF6 <- aggregate(syll.count ~ group, DF5, mean)
    DF6$ave.sent.per.100 <- aggregate(sent.per.100 ~ group, DF5, 
        mean)[, 2]
    suppressWarnings(plot(1, 1, xlim = c(108, 182), ylim = c(2, 
        25), axes = FALSE, type = "n", 
        xlab = "Average number of syllables per 100 words", 
        ylab = "Average number of sentences per 100 words", 
        main = "Fry Graph for Estimating Reading Ages (grade level)", 
        xaxs = "i", yaxs = "i"))
    axis(1, at = 108:182, labels = TRUE)
    axis(1, at = seq(108, 182, by = 4), tcl = -1.1, labels = FALSE)
    axis(2, at = 2:25, labels = TRUE)
    axis(4, at = 2:25, labels = TRUE)
    grid(nx = 74, ny = 46, lty = "solid", col = "gold")
    grid(nx = 37, ny = 23, lty = "solid", col = "gray65")
    box()
    segments(108, 9.97, 133.9, 25, lwd = 2, col = "darkgreen")
    segments(108, 7.7, 142.1, 25, lwd = 2, col = "darkgreen")
    segments(108, 6.4, 147.8, 25, lwd = 2, col = "darkgreen")
    segments(108, 5.61, 155, 25, lwd = 2, col = "darkgreen")
    segments(108, 5.1, 160, 25, lwd = 2, col = "darkgreen")
    segments(108, 3.8, 160.8, 25, lwd = 2, col = "darkgreen")
    segments(111.5, 2, 167.5, 25, lwd = 2, col = "darkgreen")
    segments(123, 2, 169, 25, lwd = 2, col = "darkgreen")
    segments(136.3, 2, 169, 25, lwd = 2, col = "darkgreen")
    segments(143.3, 2, 173.5, 25, lwd = 2, col = "darkgreen")
    segments(148.15, 2, 178.2, 25, lwd = 2, col = "darkgreen")
    segments(155.6, 2, 178.5, 25, lwd = 2, col = "darkgreen")
    segments(161.9, 2, 178.4, 25, lwd = 2, col = "darkgreen")
    segments(168.2, 2, 178.2, 25, lwd = 2, col = "darkgreen")
    segments(173.9, 2, 178.7, 25, lwd = 2, col = "darkgreen")
    segments(179.1, 2, 181.25, 25, lwd = 2, col = "darkgreen")
    x1 <- c(108, 108, 128)
    y1 <- c(4.2, 2, 2)
    polygon(x1, y1, col = "darkblue", border = "darkblue")
    x2 <- c(143, 145, 150, 152, 155, 158, 162, 166, 172, 180, 
        181, 182, 182)
    y2 <- c(25, 18.3, 14.3, 12.5, 11.1, 10, 9.1, 8.3, 7.7, 7.55, 
        7.5, 7.5, 25)
    polygon(x2, y2, col = "darkblue", border = "darkblue")  
    text(120, 22.2, 1, col = "darkgreen", cex = 1.25)
    text(124, 17.9, 2, col = "darkgreen", cex = 1.25)
    text(126, 15.9, 3, col = "darkgreen", cex = 1.25)
    text(127, 14.4, 4, col = "darkgreen", cex = 1.25)
    text(128, 13.3, 5, col = "darkgreen", cex = 1.25)
    text(110.7, 5.5, 6, col = "darkgreen", cex = 1.25)
    text(115, 4.88, 7, col = "darkgreen", cex = 1.25)
    text(121, 3.78, 8, col = "darkgreen", cex = 1.25)
    text(132, 3.5, 9, col = "darkgreen", cex = 1.25)
    text(142, 3.5, 10, col = "darkgreen", cex = 1.25)
    text(147.7, 3.5, 11, col = "darkgreen", cex = 1.25)
    text(153.7, 3.5, 12, col = "darkgreen", cex = 1.25)
    text(160, 3.5, 13, col = "darkgreen", cex = 1.25)
    text(166.4, 3.5, 14, col = "darkgreen", cex = 1.25)
    text(171, 3.5, 15, col = "darkgreen", cex = 1.25)
    text(176.7, 3.5, 16, col = "darkgreen", cex = 1.25)
    text(180.7, 3.5, 17, col = "darkgreen", cex = 1.25)
    with(DF6, points(syll.count, ave.sent.per.100, pch = 19, 
        cex = 1, col = "red"))
    with(DF6, points(syll.count, ave.sent.per.100, pch = 3, cex = 3, 
        col = "black"))   
    switch(labels, 
            click = {with(DF6, identify(syll.count, ave.sent.per.100, 
                    group))}, 
            automatic = {with(DF6, text(syll.count, ave.sent.per.100, 
                    labels = group, adj = c(1, -0.5)))}
    )      
    names(DF6) <- c(G, "ave.syll.per.100", "ave.sent.per.100")
    list(SENTENCES_USED = DF5, SENTENCE_AVERAGES = DF6)
}
