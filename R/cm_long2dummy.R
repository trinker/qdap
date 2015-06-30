#' Stretch and Dummy Code cm_xxx2long  
#' 
#' Stretches and dummy codes a cm_xxx2long dataframe to allow for combining 
#' columns.
#' 
#' @param dataframe  A dataframe that contains the person variable.
#' @param rm.var An optional character argument of the name of a repeated 
#' measures column.
#' @param code A character argument of the name of a repeated measures column.  
#' Default is \code{"code"}.
#' @param start A character argument of the name of a repeated measures column.  
#' Default is \code{"start"}.
#' @param end A character argument of the name of a repeated measures column.  
#' Default is \code{"end"}.
#' @return Returns a dataframe or a list of stretched and dummy coded 
#' dataframe(s).
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_time2long}},
#' \code{\link[qdap]{cm_df2long}}
#' @keywords dummy coded
#' @export
#' @examples
#' \dontrun{
#' foo <- list(
#'     AA = qcv(terms="1:10"),
#'     BB = qcv(terms="1:2, 3:10, 19"),
#'     CC = qcv(terms="1:3, 5:6")
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms="4:8"),
#'     BB = qcv(terms="1:4, 10:12"),
#'     CC = qcv(terms="1, 11, 15:20"),
#'     DD = qcv(terms="")
#' )
#' 
#' (x <- cm_range2long(foo))
#' cm_long2dummy(x)
#' 
#' (z <- cm_range2long(foo, foo2, v.name="time"))
#' out <- cm_long2dummy(z, "time")
#' ltruncdf(out)
#' }
cm_long2dummy <- 
function(dataframe, rm.var = NULL, 
    code = "code", start = "start", end = "end") {

    com <- which.cm(dataframe)
    if (is.null(com) && !methods::is(dataframe, "cmspans")){
        stop("Please supply an object from `cm_range2long`, `cm_time2long`, or `cm_df2long`.")
    }
      
    if (!is.null(rm.var)) {
        L1 <- split(dataframe, dataframe[, rm.var])
    } else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }

    L2 <- lapply(L1, function(x) dummy(dat=x, code = code, start = start, end = end)) 
    if (is.null(rm.var)) {
        L2 <- L2[[1]]
    }
    class(L2) <- c(sprintf("l2d_%s", com), class(L2))
    L2
}

## Helper functions with `cm_long2dummy`
span2dummy <- function(s, e, n=(1 + max(e))) {
    x <- rep(0, n)
    spans <- e-s
    ones <- unique(unlist(lapply(seq_along(s), function(i) {
        (s[i] + 1):(s[i] + spans[i])
    })))
    x[ones] <- 1
    x 
}

dummymatrix <- function(x, group.var = "code", end.var = "end") {
    cn <- unique(x[, group.var])
    nc <- length(cn)
    nr <- max(x[, end.var] + 1)
    matrix(rep(0, nc * nr), ncol = nc)
    mat <- matrix(rep(0, nc * nr), ncol = nc) 
    rownames(mat) <- 0:max(x[, end.var])#maybe remove later
    colnames(mat) <- cn
    mat 
}

dummy <- function(dat, code, start, end){

    dat[, code] <- factor(dat[, code]) ## added 0n 11/5/2013

    L2 <- split(droplevels(dat), dat[, code])
    inc <- function(dataframe, start, end) {
        any(diff(c(apply(dataframe[, c(start, end)], 1, c))) < 0)
    }
    if(any(sapply(L2, function(x) inc(x, start = start, end = end)))) {
        stop("Code values not increasing.  Possible missing rm.var argument.")
    }
    mat <- dummymatrix(dat, group.var = code, end.var = end)
    nr <- nrow(mat) 
    for (i in seq_along(L2)) {
        sect <- L2[[i]]
        mat[, names(L2)[i]] <- span2dummy(s = sect[, start], sect[, end], nr)
    }
    mat
}
