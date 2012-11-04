cm_distance <- 
function(dataframe, time.var = NULL, code.var = "code",
    causal = FALSE, start.var = "start", end.var = "end", mean.digits = 2, 
    sd.digits = 2, stan.digits = 2) {
    if (!is.null(time.var)) {
        L1 <- split(dataframe, dataframe[, time.var])
    } else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }   
    L2 <- lapply(L1, function(x) split(x, x[, code.var]))   
    NMS <- lapply(L2, names)
    NMS <- unlist(lapply(seq_along(NMS), function(i) {
        paste(names(NMS)[i], NMS[[i]], sep=".")
    }))
    lens <- sapply(L2, length)
    mlens <- sapply(L2, function(x) {
        max(do.call(rbind, x)[, end.var])
    })
    nt <- rep(mlens, lens)
    v <- unlist(L2, recursive=FALSE)
    L3 <- lapply(seq_along(v), function(i) {
        cm_se2vect(v[[i]][, start.var], v[[i]][, end.var])
    })
    L4 <- lapply(seq_along(L3), function(i){
        c(L3[[i]], rep(0, nt[i] - length(L3[[i]])))
    })
    dat <- do.call(cbind, L4)  
    colnames(dat) <- NMS
    INDS <- cumsum(lens)
    INDS2 <- c(1, INDS +1); INDS2 <- INDS2[-length(INDS2)] 
    L5 <- lapply(seq_along(INDS), function(i) dat[, INDS2[i]:INDS[i]])
    names(L5) <- names(L2)
    L5 <- lapply(L5, function(x){
        colnames(x) <- unlist(strsplit(colnames(x), "\\."))[c(F, T)]
        return(x)
    })
    if (causal) {inds <- 4} else {inds <- 2}
    dism <- function(x, y) {cm_bidist(x, y)[[inds]][1]}
    dissd <- function(x, y) {cm_bidist(x, y)[[inds]][2]}
    disn <- function(x, y) {cm_bidist(x, y)[[inds]][3]}
    FUN <- function(dat, mdigs=mean.digits, sdigs=sd.digits){
        dat <- data.frame(dat)
        means <- v.outer(dat, dism, digits=mdigs)
        sds <- v.outer(dat, dissd, digits=sdigs)
        ns <- v.outer(dat, disn, digits=0)
        DIM <- dim(means)
        pfun <- function(x, y, z) paste0(x, "(", y, ")", "n=", z)
        comb <- mgsub(c("(NA)", "NA;n=0"), c(";", NA), 
            mapply(pfun, means, sds, ns), fixed=TRUE)
        dim(comb) <- DIM
        dimnames(comb) <- list(rownames(means), colnames(means))
        diag(comb) <- gsub("0(0)", "", diag(comb), fixed=TRUE)
        stand <- round(means/sds, digits=stan.digits)
        stand[is.nan(stand)] <- NA
        list(mean=means, sd=sds, n=ns, combined=noquote(comb), standardized=stand)
    }
    o <- lapply(L5, FUN)
    class(o) <- "cm.dist"
    return(o)
}