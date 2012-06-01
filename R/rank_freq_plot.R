rank_freq_plot <-
function(words, frequencies, title.ext = NULL,  
    plot = TRUE, jitter.ammount = 0.1, log.scale = TRUE, hap.col = "red", 
    dis.col = "blue") {
    original.data <- data.frame(words = words, freq = frequencies)
    X <- tapply(words, frequencies, function(x) length(x))
    ZIPF <- data.frame(n.words = as.numeric(rev(X)), 
        freq = as.numeric(rev(rownames(X))), 
        rank = as.numeric(rev(nrow(X):1)))
    rownames(ZIPF) <- 1:nrow(ZIPF)
    title <- if (is.null(title.ext)) 
        NULL else paste(c("for", title.ext), collapse = " ")
    ZIPF2 <- ZIPF[rep(seq(dim(ZIPF)[1]), ZIPF$n.words), ]
    ZIPF <- transform(ZIPF, per.of.tot = round(freq/sum(ZIPF2$freq) * 
        100, digits = 3))
    if (plot) {
        if (log.scale) {
            with(ZIPF2, plot(jitter(log(rank), amount = jitter.ammount), 
                log(freq), ylab = "Frequency (log scale)", 
                xlab = "Rank (log scale)", 
                main = paste("Rank-Frequency Plot", title, collapse = " "), 
                col = ifelse(freq == 1, hap.col, ifelse(freq == 2, 
                dis.col, "black"))))
        } else {
            with(ZIPF2, plot(jitter(rank, amount = jitter.ammount), 
                freq, ylab = "Frequency", xlab = "Rank", 
                main = paste("Rank-Frequency Plot", 
                title, collapse = " "), col = ifelse(freq == 1, 
                hap.col, ifelse(freq == 2, dis.col, "black"))))
        }
    } else {
        NULL
    }
    percent_hapax_legomena <- round(ZIPF[ZIPF$freq == 1, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    percent_dis_legomena <- round(ZIPF[ZIPF$freq == 2, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    list(ORIGINAL_DATA = original.data, RANK_AND_FREQUENCY_STATS = ZIPF, 
        LEGOMENA_STATS = c(percent_hapax_legomena = percent_hapax_legomena, 
        percent_dis_legomena = percent_dis_legomena))
}
