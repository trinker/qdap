cm_long2dummy <-
function(dataframe, rm.var = NULL, 
    code = "code", start = "start", end = "end") {
    if (!is.null(rm.var)) {
        L1 <- split(dataframe, dataframe[, rm.var])
    } else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }
    dummy <- function(dat, code, start, end){
        L2 <- split(dat, dat[, code])
        inc <- function(dataframe, start, end) {
            any(diff(c(apply(dataframe[, c(start, end)], 1, c))) < 0)
        }
        if(any(sapply(L2, function(x) inc(x, start = start, end = end)))) {
            stop("Code values not increasing.  Possible missing rm.var argument.")
        }
        nr <- max(sapply(L2, function(x) max(x[, end])))
        nc <- length(L2)
        mat <- matrix(rep(0, nr*nc), ncol=nc)
        colnames(mat) <- names(L2)
        yes <- lapply(L2, function(x) {
            c(unique(unlist(sapply(1:nrow(x), 
                function(i) x[i, start]:x[i, end]))))
        })
        invisible(lapply(seq_along(yes), function(i){
            mat[yes[[i]], i] <<- 1
        }))
        data.frame(mat)
    }
    lapply(L1, function(x) dummy(dat=x, code = code, start = start, end = end))
}