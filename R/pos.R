pos <-
function(text.var, na.omit = FALSE, digits = 2, 
    progress.bar = TRUE, gc.rate=10){
    suppressWarnings(require(openNLP))
    suppressWarnings(require(openNLPmodels.en))
    pos <-  function(i) {
        x <- openNLP::tagPOS(strip(i))   
        return(x)
    }
    ntv <- length(text.var)
    if (progress.bar != FALSE){
        if (Sys.info()[['sysname']] == "Windows" & progress.bar != "text"){
            pb <- winProgressBar(title = "progress bar", min = 0,
                max = ntv, width = 300)
            m <- sapply(seq_len(ntv), function(i) {
                    x <- pos(text.var[i])
                    if (i%%gc.rate==0) gc()
                    setWinProgressBar(pb, i, title = paste(round(i/ntv*100, 0),
                        "% done"))
                    return(x)
                }
            )
            close(pb)
        } else {
            pb <- txtProgressBar(min = 0, max = ntv, style = 3)
            m <- sapply(seq_len(ntv), function(i) {
                    x <- pos(text.var[i])
                    if (i%%gc.rate==0) gc()
                    setTxtProgressBar(pb, i)
                    return(x)
                }
            )
            close(pb)
        }
    } else {
        m <- sapply(seq_len(ntv), function(i) {
                x <- pos(text.var[i])
                if (i%%gc.rate==0) gc()
                return(x)
            }
        )

    }
    names(m) <- NULL
    poser <- function(x) sub("^.*/([^ ]+).*$","\\1", 
        unlist(strsplit(x, " ")))
    o <- lapply(m, poser)
    lev <- sort(unique(unlist(o)))
    G4 <- do.call(rbind,lapply(o,function(x,lev){ 
            tabulate(factor(x,levels = lev, ordered = TRUE),
            nbins = length(lev))},lev = lev))
    colnames(G4) <-sort(lev)
    m <- data.frame(POStagged = m)
    m$POStags <- o 
    m$word.count <- word.count(text.var)
    G5 <- sapply(data.frame(G4, check.names = FALSE), 
        function(x) round(x/m$word.count, digits = digits)) 
    colnames(G5) <- paste0("prop", colnames(G5))
    G4 <- data.frame(wrd.cnt = m$word.count, G4, check.names = FALSE)
    G5 <- data.frame(wrd.cnt = m$word.count, G5, check.names = FALSE)
    if (any(is.na(G4$wrd.cnt))) {
        nas <- which(is.na(G4$wrd.cnt))
        G4[nas, 2:ncol(G4)] <- NA
        m[nas, 1:ncol(m)] <- NA
    }
    POS <- list(POStagged = m, POSprop = G5, POSfreq = G4)
    lapply(seq_along(POS), function(i) {
        comment(POS[[i]]) <<- names(POS)[[i]]
       }
    )
    if(na.omit) POS <- lapply(POS, na.omit)
    comment(POS) <- "POS"
    return(POS)
}
