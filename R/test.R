#' Stretch and Dummy Code cm_xxx2long  
#' 
#' Stretches and dummy codes a cm_xxx2long dataframe to allow for combining columns.
#' 
#' @param dataframe  A dataframe that contains the person variable.
#' @param rm.var An optional character argument of the name of a repeated measures column.
#' @param code A character argument of the name of a repeated measures column.  Default is "code".
#' @param start A character argument of the name of a repeated measures column.  Default is "start".
#' @param end A character argument of the name of a repeated measures column.  Default is "end".
#' @return Returns a stretched and dummy coded dataframe
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qda]{cm_time2long}},
#' \code{\link[qda]{cm_df2long}}
#' @keywords dummy coded
#' @examples
#' foo <- list(
#'     AA = qcv(terms='1'),
#'     BB = qcv(terms='1:2, 3:10, 19'),
#'     CC = qcv(terms='1:3, 5:6')
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms='4'),
#'     BB = qcv(terms='10:12'),
#'     CC = qcv(terms='1, 11, 15:20'),
#'     DD = qcv(terms='')
#' )
#' 
#' x <- cm_range2long(foo)
#' cm_long2dummy(x)
#' 
#' z <- cm_range2long(foo, foo2, v.name="time")
#' cm_long2dummy(z, "time")
test <-
function(dataframe, rm.var = NULL, 
    code = "code", start = "start", end = "end") {
    if (!is.null(rm.var)) {
        L1 <- split(dataframe, dataframe[, rm.var])
    } else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }
    dummy <- function(dat, codeb, startb, endb){
        L2 <- split(dat, dat[, codeb])
        inc <- function(dataframe, startc, endc) {
            any(diff(c(apply(dataframe[, c(startb, endb)], 1, c))) < 0)
        }
        if(any(sapply(L2, function(x) inc(x, startc = startb, endc = endb)))) {
            stop("Codeb values not increasing.  Possible missing rm.var argument.")
        }
        nr <- max(sapply(L2, function(x) max(x[, endb])))
        nc <- length(L2)
        mat <- matrix(rep(0, nr*nc), ncol=nc)
        colnames(mat) <- names(L2)
        yes <- lapply(L2, function(x) {
            c(unique(unlist(sapply(1:nrow(x), 
                function(i) x[i, startb]:x[i, endb]))))
        })
        invisible(lapply(seq_along(yes), function(i){
            mat[yes[[i]], i] <<- 1
        }))
        data.frame(mat)
    }
    lapply(L1, function(x) dummy(dat=x, codeb = code, startb = start, endb = end))
}