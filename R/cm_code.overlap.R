#' Find Co-occurrence Between Codes
#'
#' Combine co-occurrences of codes into a new code.
#'
#' @param x2long.obj An object from \code{\link[qdap]{cm_range2long}}, 
#' \code{\link[qdap]{cm_time2long}} or \code{\link[qdap]{cm_df2long}}.
#' @param overlap.code.list A list of named character vectors of at least two 
#' code column names to aggregate co-occurrences.
#' @param rm.var Name of the repeated measures column.  
#' @return Returns a dataframe with co-occurrences of supplied overlapping codes 
#' added.
#' @note The code column must be named code and your start and end columns must 
#' be named \code{"start"} and \code{"end"}.
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_time2long}},
#' \code{\link[qdap]{cm_df2long}},
#' \code{\link[qdap]{cm_code.combine}},
#' \code{\link[qdap]{cm_code.transform}}
#' @keywords co-occurrence
#' @export
#' @importFrom qdapTools sec2hms
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
#' cm_code.overlap(x, list(AB=qcv(AA, BB)))
#' cm_code.overlap(x, list(ALL=qcv(AA, BB, CC)))
#' combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' (a <- cm_code.overlap(z, combines, "time"))
#' plot(a)
#' 
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
#' out <- cm_code.overlap(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)), 
#'     rm.var="variable")
#' head(out, 10)
#' }
cm_code.overlap <- function(x2long.obj, overlap.code.list, rm.var = NULL) {
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
       
    ## added 11-6-13 to deal with codes in rep mes. not found in both
    cols <- unique(unlist(lapply(x1, colnames)))
    if (!is.matrix(x1)) {
        x1 <- lapply(x1, function(x, y =cols) {
            addon <- y[!y %in% colnames(x)]
            if (is.null(addon) | identical(addon, character(0))) return(x)
            mat <- matrix(rep(0, nrow(x)*length(addon)), ncol = length(addon))
            colnames(mat) <- addon
            dat <- data.frame(x, mat)
            dat[, sort(colnames(dat))]
        })
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
    if (which.cm(x2long.obj) == "cmtime") {
        ## DF$start <- DF$start + 1  #removed 9-25-2013
        DF$Start <- sec2hms(DF$start)
        DF$End <- sec2hms(DF$end) 
        DF <- data.frame(DF[, -4, drop=FALSE], DF[, 4, drop=FALSE])
        class(DF) <- class(DF)[!grepl("vname_", class(DF))]
        class(DF) <- c("cmspans", which.cm(x2long.obj), 
            paste0("vname_", rm.var), class(DF))
    }
    if (rmv) {
        DF$time <- NULL
        class(DF) <- class(DF)[!grepl("vname_", class(DF))]
    }
    DF
}
