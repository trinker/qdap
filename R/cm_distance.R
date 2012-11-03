#' Distance Matrix Between Codes 
#' 
#' Generate distance measures to assertain a mean distance emasure between codes.
#' 
#' @aliases word_list qda print.qda
#' @param dataframe a data frame from the cm_x2long family (cm_range2long; cm_df2long; cm_time2long)
#' @param time.var an optional variable to split the dataframe by (if you have data that is by various times this must be supplied).
#' @param code.var the name of the code variable column.  Defaults to "codes" as out putted by x2long family
#' @param causal logical.  If TRUE measures the distance ebtween x and y given that x must procede y
#' @param start.var the name of the start variable column.  Defaults to "start" as out putted by x2long family
#' @param end.var the name of the end variable column.  Defaults to "end" as out putted by x2long family
#' @param mean.digits the number of digits to be displayed in the mean matrix
#' @param sd.digits the number of digits to be displayed in the sd matrix
#' @return An object of the class cm.dist.  This is a list of n lists with the following components per each list (time.var): 
#' \item{mean}{A distance matrix of average distances between codes}
#' \item{sd}{A matrix of standard deviations of distances between codes}
#' \item{n}{A matrix of counts of distances between codes}
#' \item{combined}{A matrix of combined mean, sd and n of distances between codes}
#' \item{standardized}{A matrix of standardized values of distances between codes}
#' @keywords distance
#' @examples
#' foo <- list(
#'     AA = qcv(terms='02:03, 05'),
#'     BB = qcv(terms='1:2, 3:10'),
#'     CC = qcv(terms='1:9, 100:150')
#' )
#' foo2  <- list(
#'     AA = qcv(terms='40'),
#'     BB = qcv(terms='50:90'),
#'     CC = qcv(terms='60:90, 100:120, 150'),
#'     DD = qcv(terms='')
#' )
#' (dat <- cm_range2long(foo, foo2, v.name = "time"))
#' (out <- cm_distance(dat, time.var = "time", causal=T))
#' names(out)
#' names(out$foo2)
#' out$foo2
cm_distance <-
function(dataframe, time.var = NULL, code.var = "code",
    causal = FALSE, start.var = "start", end.var = "end", mean.digits = 2, 
    sd.digits = 2, stan.digits = 2) {
    if (!is.null(time.var)) {
        L1 <- split(dataframe, dataframe[, time.var])
    } else {
        L1 <- dataframe
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
    L5 <- lapply(seq_along(INDS), function(i) dat[, INDS[i]:INDS2[i]])
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
        stand <- round(means/sds, digits=stan.digits)
        stand[is.nan(stand)] <- NA
        list(mean=means, sd=sds, n=ns, combined=noquote(comb), standardized=stand)
    }
    lapply(L5, FUN)
    o <- lapply(L5, FUN)
    class(o) <- "cm.dist"
    return(o)
}
