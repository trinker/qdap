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
#' If \code{TRUE} finds the overlap.  If \code{FALSE} finds anywhere any of the 
#' codes occur.  If integer finds that exact combination of overlaps.  If 
#' character must be a logical vector 
#' c(\code{>}, \code{<}, \code{=<}, \code{=>}, \code{==}, \code{!=}) followed by 
#' an integer and wrapped with quotes.
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
#' ## Single occurrence version
#' (x <- cm_range2long(foo))
#' 
#' cm_code.blank(x, combine.code.list = list(ABC=qcv(AA, BB, CC)),
#'     overlap = "!=1")
#' 
#' ## Repeated measures version
#' (z <- cm_range2long(foo, foo2, v.name="time"))
#' 
#' cm_code.blank(z, combine.code.list = list(ABC=qcv(AA, BB, CC)),
#'     rm.var = "time", overlap = "!=1")
#' 
#' cm_code.blank(z, combine.code.list = list(AB=qcv(AA, BB)),
#'     rm.var = "time", overlap = TRUE)
#' 
#' cm_code.blank(z, combine.code.list = list(AB=qcv(AA, BB)),
#'     rm.var = "time", overlap = FALSE)
#' 
#' cm_code.blank(z, combine.code.list = list(AB=qcv(AA, BB)),
#'     rm.var = "time", overlap = ">1")
#' 
#' cm_code.blank(z, combine.code.list = list(AB=qcv(AA, BB)),
#'     rm.var = "time", overlap = "==2")
#' 
#' ## Notice `overlap = "==2"` above is identical to `cm_code.overlap`
#' cm_code.overlap(z, overlap.code.list = list(AB=qcv(AA, BB)),
#'     rm.var = "time")
#' 
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
#' dat <- cm_time2long(x, y, v.name="time")
#' head(dat, 10)
#' out <- cm_code.blank(dat, combine.code.list = list(ABC=qcv(A, B, C)),
#'     rm.var = "time", overlap = "!=1")
#' 
#' head(out)
#' plot(out)
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
    
    ## added 11-6-13 to deal with codes in rep mes. not found in both
    cols <- unique(unlist(lapply(x1, colnames)))
    if (!is.matrix(x1)) {
        x1 <- lapply(x1, function(x, y =cols) {
            addon <- y[!y %in% colnames(x)]
            if (is.null(addon) | identical(addon, character(0))) return(x)
            mat <- matrix(rep(0, nrow(x)*length(addon)), ncol = length(addon))
            colnames(mat) <- addon
            dat <- data.frame(x, mat, stringsAsFactors = FALSE)
            dat[, sort(colnames(dat))]
        })
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
    if (which.cm(x2long.obj) == "cmtime") {
        DF$start <- DF$start + 1
        DF$Start <- sec2hms(DF$start)
        DF$End <- sec2hms(DF$end) 
        DF <- data.frame(DF[, -4, drop=FALSE], DF[, 4, drop=FALSE], stringsAsFactors = FALSE)
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
