#' Blank Code Transformation
#' 
#' Transform codes with any binary operator combination.
#' 
#' @param x2long.obj An object from \code{\link[qdap]{cm_range2long}}, 
#' \code{\link[qdap]{cm_time2long}} or \code{\link[qdap]{cm_df2long}}.
#' @param combine.code.list A list of named character vectors of at least two 
#' code column names to combine.
#' @param rm.var Name of the repeated measures column. 
#' @param overlap logical, integer or character of binary operator + integer.  
#' If TRUE finds the overlap.  If FALSE finds anywhere any of the codes occur.  
#' If integer finds that exact combination of overlaps.  If character must be a 
#' logical vector c(>, <, =<, =>, ==, !=) followed by an integer and wrapped 
#' with quotes.
#' @return Returns a dataframe with transformed occurrences of supplied 
#' overlapping codes added.
#' @note For most jobs \code{\link[qdap]{cm_code.transform}} will work.  This
#' adds a bit of flexibility in exclusion and partial matching.  The code column 
#' must be named \code{"code"} and your start and end columns must be named 
#' \code{"start"} and \code{"end"}.
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_time2long}},
#' \code{\link[qdap]{cm_df2long}},
#' \code{\link[qdap]{cm_code.overlap}},
#' \code{\link[qdap]{cm_code.combine}},
#' \code{\link[qdap]{cm_code.exclude}},
#' \code{\link[qdap]{cm_code.transform}}
#' @keywords co-occurrence
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
#' 
#' (x <- cm_range2long(foo))
#' (z <- cm_range2long(foo, foo2, v.name="time"))
#' cm_code.transform(x, overlap.code.list=list(AB=qcv(AA, BB)))
#' cm_code.transform(x, combine.code.list = list(ALL=qcv(AA, BB, CC)))
#' cm_code.transform(x, overlap.code.list=list(AB=qcv(AA, BB)), 
#'     combine.code.list = list(ALL=qcv(AA, BB, CC)))
#' overlaps <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' cm_code.transform(z, overlaps, rm.var="time")
#' out1 <- cm_code.transform(z, overlaps, 
#'    exclude.code.list=list(AABB_no_CC = qcv(AA, BB, CC)), rm.var="time")
#' head(out1, 10)
#' #WITH cm_time2long
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 
#'         1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' 
#' y <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 
#'         1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' 
#' dat <- cm_time2long(x, y)
#' head(dat, 10)
#' out2 <- cm_code.transform(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)), 
#'     list(S=qcv(A, B), T=qcv(B, C), U=qcv(A, B, C)), 
#'     list(ABnoC = qcv(A, B, C)), rm.var="variable")
#' head(out2, 10)
#' }
cm_code.blank <- function(x2long.obj, combine.code.list, rm.var = NULL, overlap = TRUE) {
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
    x2 <- cm_combine.dummy(x1, combine.code = combine.code.list, 
        rm.var = rm.var, overlap = overlap)
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
        DF$Start <- sec2hms(DF$start)
        DF$End <- sec2hms(DF$end) 
        DF <- data.frame(DF[, -4, drop=FALSE], DF[, 4, drop=FALSE])
    }
    comment(DF) <- comment(x2long.obj)
    if (rmv) {
        DF$time <- NULL
    }
    DF
}
