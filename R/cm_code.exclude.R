#' Exclude Codes
#'
#' Find the occurrences of n codes excluding the nth code.  e.g.  You have 
#' times/words coded for a teacher and you also have times/words coded for 
#' happiness.  You can find all the happiness times excluding the teacher times 
#' or vise versa.
#'
#' @param x2long.obj An object from \code{\link[qdap]{cm_range2long}}, 
#' \code{\link[qdap]{cm_time2long}} or \code{\link[qdap]{cm_df2long}}.
#' @param exclude.code.list A list of named character vectors of at least two 
#' code column names to compare and exclude.  The last column name is the one 
#' that will be excluded.
#' @param rm.var Name of the repeated measures column. 
#' @return Returns a dataframe with n codes excluding the nth code.
#' @note The code column must be named \code{"code"} and your start and end 
#' columns must be named \code{"start"} and \code{"end"}.
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_time2long}},
#' \code{\link[qdap]{cm_df2long}},
#' \code{\link[qdap]{cm_code.blank}},
#' \code{\link[qdap]{cm_code.combine}},
#' \code{\link[qdap]{cm_code.overlap}},
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
#' cm_code.exclude(x, list(ABnoC=qcv(AA, BB, CC)))
#' cm_code.exclude(z, list(ABnoC=qcv(AA, BB, CC)), rm.var="time")
#' excludes <- list(AnoB=qcv(AA, BB), ABnoC=qcv(AA, BB, CC))
#' cm_code.exclude(z, excludes, rm.var="time")
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
#' cm_code.exclude(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)), 
#'     rm.var = "variable")
#' }
cm_code.exclude <- function(x2long.obj, exclude.code.list, rm.var = NULL) {
    if (!is.null(rm.var)) {
        if(!rm.var %in% colnames(x2long.obj)) {
            stop("rm.var does not match a column")
        }
    }
    exclude.code.list1 <- exclude.code.list
    names(exclude.code.list1) <- paste0(names(exclude.code.list), "_rmvme123")
    exclude.code.list2 <- invisible(lapply(seq_along(exclude.code.list), function(i) {
        c(names(exclude.code.list1)[i], 
        exclude.code.list[[i]][length(exclude.code.list[[i]])])
    }))
    names(exclude.code.list2) <- names(exclude.code.list)
    w <- cm_code.combine(x2long.obj, exclude.code.list1, rm.var=rm.var)
    z <- cm_code.blank(w, exclude.code.list2, overlap=1, rm.var=rm.var)
    if (ncol(x2long.obj) != ncol(z)){
        v <- x2long.obj[!colnames(x2long.obj) %in% colnames(z)]
        z$newname1234 <- rep(v[1, 1], nrow(z))
        colnames(z)[colnames(z) == "newname1234"] <- colnames(v)
    }
    dat <- data.frame(rbind(x2long.obj, z[z$code %in% names(exclude.code.list),]), row.names=NULL)
    if (!is.null(rm.var)) {
        dat[order(dat[rm.var]), ]
    } else {
        dat
    }
}
