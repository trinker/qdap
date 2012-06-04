word.comp <-
function(x, y, wordlist = NULL, stopwords = NULL, 
    raw.digits = 2, prop.digits = 4, alert = TRUE) {
    NAMEx <- as.character(substitute(x))
    NAMEx <- NAMEx[length(NAMEx)]
    NAMEy <- as.character(substitute(y))
    NAMEy <- NAMEy[length(NAMEy)]
    x <- qda.handler(x)
    y <- qda.handler(y)
    Dx <- data.frame(text.var = x, grouping.var = NAMEx)
    Dy <- data.frame(text.var = y, grouping.var = NAMEy)
    DF <- data.frame(rbind(Dx, Dy))
    i <- data.frame(wfm(text.var = DF$text.var, 
        grouping.var = DF$grouping.var, stopwords = stopwords))
    i$diff <- c(i[, 2] - i[, 1])
    i <- if (!is.null(stopwords)) i[!rownames(i) %in% stopwords, ]
    prop.vect <- function(x, digits = prop.digits) round(x/sum(x), digits)
    i <- transform(i, prop.diff=prop.vect(i[, 2]) - prop.vect(i[, 1]))
    i$perc.alert <- if (!alert) {
        NULL
    } else {
        x <- abs(scale(i[, "prop.diff"]))
        as.factor(ifelse(x > 4, "> 4 sd", 
           ifelse(x > 3, "> 3 sd", 
           ifelse(x > 2, "> 2 sd", 
           ifelse(x > 1, "> 1 sd", "-")))))
    }
    N1 <- sum(i[, 1]!=0)
    N2 <- sum(i[, 2]!=0)
    i[, "diff.effect"] <- round(c(i$diff)/sqrt(N1 + N2 + (N2 - N1)), 
        digits = raw.digits)
    i$effect.alert <- if (!alert) {
        NULL
    } else {
        x <- abs(scale(i[, "diff.effect"]))
        as.factor(ifelse(x > 4, "> 4 sd", 
           ifelse(x > 3, "> 3 sd", 
           ifelse(x > 2, "> 2 sd", 
           ifelse(x > 1, "> 1 sd", "-")))))
    }
    CH <- suppressWarnings(chisq.test(i[, names(i)[1:2]]))
    i$stdres <- round(CH$stdres[, 2], digits = raw.digits)
    i$`O-E`<- round((CH$observed - CH$expected)[, 2], digits = raw.digits)
    i$`(O-E)^2/E`<- round((((CH$observed - CH$expected)[, 2]^2)/
       (CH$expected[, 2])), digits = raw.digits)
    chi <- data.frame(do.call("cbind", CH[1:3]))
    names(chi)[1:2] <- c("chisq", "df")
    rownames(chi) <-" "
    u <- matrix(paste(NAMEy, "compared to", NAMEx, sep=" "))
    dimnames(u) <- list(c(""), c(""))
    u <- noquote(u)
    if (!is.null(wordlist)) k <- i[rownames(i) %in% wordlist, ] 
        if (!is.null(wordlist)) {
        o <- list(raw = i, "words of interest" = k, stat = chi, 
            comparison = u)
    } else { 
        o <- list(raw = i, stat = chi, comparison = u)
    }
    return(o)
}
