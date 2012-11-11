#' Find Co-occurrence Between Codes
#'
#' Combine co-occurances of codes into a new code.
#'
#' @param x2long.obj An object from cm_range2long, cm_time2long or cm_df2long
#' @param overlap.code.list A list of named character vertors of at least two code column names to combine
#' @param rm.var Name of the repeated measures column.  Default is "time".
#' @return Returns a dataframe with co-occurrences of supplied overlapping codes added.
#' @note The code column must be names code and your start and end columns must be named "start" and "end".
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_time2long}},
#' \code{\link[qdap]{cm_df2long}},
#' @keywords co-occurence
#' @examples
#' foo <- list(
#'     AA = qcv(terms='1:10'),
#'     BB = qcv(terms='1:2, 3:10, 19'),
#'     CC = qcv(terms='1:3, 5:6')
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms='4:8'),
#'     BB = qcv(terms='1:4, 10:12'),
#'     CC = qcv(terms='1, 11, 15:20'),
#'     DD = qcv(terms='')
#' )
#' 
#' x <- cm_range2long(foo)
#' z <- cm_range2long(foo, foo2, v.name="time")
#' combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' cm_overlap(x, list(AB=qcv(AA, BB)))
#' cm_overlap(x, list(ALL=qcv(AA, BB, CC)))
#' cm_overlap(z, combines, "time")
#' 
#' #WITH cm_time2long
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' 
#' y <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' 
#' dat <- cm_time2long(x, y)
#' cm_overlap(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)), "variable")
cm_overlap <- function(x2long.obj, overlap.code.list, rm.var = NULL) {
    NMS <- as.character(substitute(x2long.obj))
    if (!is.null(rm.var)) {
        if(!rm.var %in% colnames(x2long.obj)) {
            stop("rm.var does not match a column")
        }
    }
    x1 <- cm_long2dummy(x2long.obj, rm.var = rm.var)
    if (is.null(rm.var)) {
        colnames(x1) <- gsub("long.obj.", "", colnames(x1))
    }
    x2 <- cm_combine.dummy(x1, combine.code = overlap.code.list, rm.var = rm.var)
    if (is.null(rm.var)) {
        x2$time <- NMS
    }
    rmv <- FALSE
    if (is.null(rm.var)) {
        rmv <- TRUE
        rm.var <- "time"
    }
    DF <- cm_dummy2long(x2, rm.var = rm.var)
    if (comment(x2long.obj) == "cmtime") {
        DF$start <- DF$start + 1
        DF$Start <- convert(DF$start)
        DF$End <- convert(DF$end) 
    }
    comment(x2long.obj)
    comment(DF) <- comment(x2long.obj)
    if (rmv) {
        DF$time <- NULL
    }
    DF
}
