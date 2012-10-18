#' Transcript Apply Parts of Speech
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param na.omit %% ~~Describe \code{na.omit} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @param progress.bar %% ~~Describe \code{progress.bar} here~~
#' @param gc.rate %% ~~Describe \code{gc.rate} here~~
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
pos <-
function(text.var, parallel = FALSE, na.omit = FALSE, digits = 2, 
    progress.bar = TRUE, gc.rate=10){
    ntv <- length(text.var)    
    pos1 <-  function(i) {
        x <- openNLP::tagPOS(qdap::strip(i))   
        return(x)
    }
    if (parallel){
        cl <- parallel::makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "ntv", "gc.rate", 
            "pos1"), envir = environment())
        m <- parallel::parLapply(cl, seq_len(ntv), function(i) {
                x <- pos1(text.var[i])
                if (i%%gc.rate==0) gc()
                return(x)
            }
        )
        parallel::stopCluster(cl)
        m <- unlist(m)
    } else {
        if (progress.bar != FALSE){
            if (Sys.info()[['sysname']] == "Windows" & progress.bar != "text"){
                pb <- winProgressBar(title = "progress bar", min = 0,
                    max = ntv, width = 300)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- pos1(text.var[i])
                        if (i%%gc.rate==0) gc()
                        setWinProgressBar(pb, i, title = paste(round(i/ntv*100, 0),
                            "% done"))
                        return(x)
                    }
                )
                close(pb)
                m <- unlist(m)
            } else {
                pb <- txtProgressBar(min = 0, max = ntv, style = 3)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- pos1(text.var[i])
                        if (i%%gc.rate==0) gc()
                        setTxtProgressBar(pb, i)
                        return(x)
                    }
                )
                close(pb)
                m <- unlist(m)
            }
        } else {
            m <- lapply(seq_len(ntv), function(i) {
                    x <- pos1(text.var[i])
                    if (i%%gc.rate==0) gc()
                    return(x)
                }
            )
            m <- unlist(m)
        }
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
    POS <- list(text = text.var, POStagged = m, POSprop = G5, POSfreq = G4)
    if(na.omit) POS <- lapply(POS, na.omit)
    class(POS) <- "POS"
    return(POS)
}