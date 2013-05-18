#' Distance Matrix Between Codes 
#' 
#' Generate distance measures to ascertain a mean distance measure between codes.
#' 
#' @param dataframe A data frame from the cm_x2long family 
#' (\code{cm_range2long}; \code{cm_df2long}; \code{cm_time2long}).
#' @param time.var An optional variable to split the dataframe by (if you have 
#' data that is by various times this must be supplied).
#' @param parallel logical.  If TRUE runs the cm_distance on multiple cores.  
#' This is effective with larger data sets but may actually be slower with 
#' smaller data sets.
#' @param code.var The name of the code variable column.  Defaults to "codes" as 
#' out putted by x2long family.
#' @param causal logical.  If TRUE measures the distance between x and y given 
#' that x must proceed y.
#' @param start.var The name of the start variable column.  Defaults to "start" 
#' as out putted by x2long family.
#' @param end.var The name of the end variable column.  Defaults to "end" as out 
#' putted by x2long family.
#' @param mean.digits The number of digits to be displayed in the mean matrix.
#' @param sd.digits The number of digits to be displayed in the sd (standard 
#' deviation) matrix.
#' @param stan.digits The number of digits to use in the standardized mean 
#' difference matrix.
#' @return An object of the class cm.dist.  This is a list of n lists with the 
#' following components per each list (time.var): 
#' \item{mean}{A distance matrix of average distances between codes}
#' \item{sd}{A matrix of standard deviations of distances between codes}
#' \item{n}{A matrix of counts of distances between codes}
#' \item{combined}{A matrix of combined mean, sd and n of distances between 
#' codes}
#' \item{standardized}{A matrix of standardized values of distances between 
#' codes.  The closer a value is to zero the closer two codes relate.}
#' @keywords distance
#' @export
#' @examples
#' \dontrun{
#' foo <- list(
#'     AA = qcv(terms="02:03, 05"),
#'     BB = qcv(terms="1:2, 3:10"),
#'     CC = qcv(terms="1:9, 100:150")
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms="40"),
#'     BB = qcv(terms="50:90"),
#'     CC = qcv(terms="60:90, 100:120, 150"),
#'     DD = qcv(terms="")
#' )
#' 
#' (dat <- cm_range2long(foo, foo2, v.name = "time"))
#' (out <- cm_distance(dat, time.var = "time", causal=TRUE))
#' names(out)
#' names(out$foo2)
#' out$foo2
#' #========================================
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00, 
#'         10.00:11.00, 59.56"),
#'     B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01")
#' )
#' (dat <- cm_time2long(x))
#' gantt_wrap(dat, "code", border.color = "black", border.size = 5, 
#'     sig.dig.line.freq = -2)
#' (a <- cm_distance(dat))
#' names(a)
#' names(a$dat)
#' a$dat
#' }
cm_distance <- 
function(dataframe, time.var = NULL, parallel = FALSE, code.var = "code",
    causal = FALSE, start.var = "start", end.var = "end", mean.digits = 2, 
    sd.digits = 2, stan.digits = 2) {
    DIST <- function(DF, CV= code.var, 
        CAU = causal, SV = start.var, EV = end.var, 
        MD = mean.digits, SDD = sd.digits,  SD = stan.digits) {  
        L2 <- split(DF, DF[, CV])
        L2 <- L2[sapply(L2, nrow) != 0] 
        NMS <- names(L2) 
        L3 <- lapply(seq_along(L2), function(i) {
            cm_se2vect(L2[[i]][, SV], L2[[i]][, EV])
        })
        lens <- max(sapply(L3, length))
        L4 <- lapply(seq_along(L3), function(i){
            c(L3[[i]], rep(0, lens - length(L3[[i]])))
        })
        dat <- do.call(cbind, L4)  
        colnames(dat) <- NMS
        if (CAU) {inds <- 4} else {inds <- 2}
        dism <- function(x, y) {cm_bidist(x, y)[[inds]][1]}
        dissd <- function(x, y) {cm_bidist(x, y)[[inds]][2]}
        disn <- function(x, y) {cm_bidist(x, y)[[inds]][3]}
        FUN <- function(dat, mdigs=MD, sdigs=SDD){
            dat <- data.frame(dat)
            means <- round(v.outer(dat, dism), digits=mdigs)
            sds <- round(v.outer(dat, dissd), digits=sdigs)
            ns <- v.outer(dat, disn)
            DIM <- dim(means)
            pfun <- function(x, y, z) paste0(x, "(", y, ")", "n=", z)
            comb <- mgsub(c("(NA)", "NA;n=0"), c(";", NA), 
                mapply(pfun, means, sds, ns), fixed=TRUE)
            dim(comb) <- DIM
            dimnames(comb) <- list(rownames(means), colnames(means))
            diag(comb) <- gsub("0(0)", "", diag(comb), fixed=TRUE)
            scale.all <- function(x) {
                dims <- dim(x)
                dnms <- dimnames(x)
                x <- matrix(scale(c(x), F), dims)
                dimnames(x) <- dnms
                x
            }
            stand <- round(scale.all(means)*scale.all(sds), 
                digits=SD)
            stand[is.nan(stand)] <- NA
            list(mean=means, sd=sds, n=ns, combined=noquote(comb), 
                standardized=stand)
        }
        FUN(dat)
    }  
    if (!is.null(time.var)) {
        L1 <- split(dataframe, dataframe[, time.var])
    } else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }  
    if (parallel){
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("dataframe", "time.var", "code.var",
        "causal", "start.var", "end.var", "mean.digits", "sd.digits", 
        "stan.digits", "cm_se2vect", "v.outer", "cm_bidist", "mgsub"), 
        envir = environment())
        o <- parLapply(cl, L1, DIST)
        stopCluster(cl)
    } else { 
        o <- lapply(L1, DIST)
    }
    class(o) <- "cm_distance"
    return(o)
}


#' Prints a cm_distance Object
#' 
#' Prints a cm_distance object.
#' 
#' @param x The cm_distance object.
#' @param \ldots ignored
#' @method print cm_distance
#' @S3method print cm_distance
print.cm_distance <-
function(x, ...){
    x <- unlist(x, recursive=F)
    y <- unlist(strsplit(names(x), "\\."))[c(FALSE, TRUE)]
    z <- x[y == "standardized"]
    invisible(lapply(seq_along(z), function(i) {
        a <- strsplit(names(z)[i], "\\.")
        if(length(unlist(a)) > 1) {
            cat(paste0(a[[1]][1], "\n"))
        } 
        cat(paste0(a[[1]][length(a[[1]])], ":\n"))
        print(z[[i]])
        cat("\n")
    }))
}
