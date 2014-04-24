#' Exclude Codes
#'
#' Find the occurrences of n codes excluding the nth code.  For example you have 
#' times/words coded for a teacher and you also have times/words coded for 
#' happiness.  You can find all the happiness times excluding the teacher times 
#' or vice versa.
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
#' cm_code.exclude(x, list(ABnoC=qcv(AA, BB, CC)))
#' cm_code.exclude(z, list(ABnoC=qcv(AA, BB, CC)), rm.var="time")
#' excludes <- list(AnoB=qcv(AA, BB), ABnoC=qcv(AA, BB, CC))
#' (a <- cm_code.exclude(z, excludes, rm.var="time"))
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
#' cm_code.exclude(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)), 
#'     rm.var = "variable")
#' }
cm_code.exclude <- 
function(x2long.obj, exclude.code.list, rm.var = NULL) {
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
    rmv <- cm_long2dummy(w, rm.var = rm.var)
    if (is.data.frame(rmv) | is.matrix(rmv)) {
        if (is.matrix(rmv)) {
            rmv <- data.frame(rmv)
        }
        colnames(rmv) <- gsub("w\\.", "", colnames(rmv))
    } else {
        for(i in 1:length(rmv)) {
            colnames(rmv[[i]]) <- gsub("w\\.", "", colnames(rmv[[i]]))
        }
    }
    if (is.data.frame(rmv)) {
        rmv <- list(rmv)
        names(rmv) <- "time1"
    }

    out2 <- lapply(rmv, function(x) {
        out <- lapply(1:length(exclude.code.list2), function(i, x2 = x) {
            y <- x2[, exclude.code.list2[[i]]]
            z <- y[, 1, drop = FALSE] - y[, 2, drop = FALSE]
            colnames(z)[1] <- gsub("_rmvme123", "", colnames(z)[1])
            z
        })
        cbind(x[, !colnames(x) %in% sapply(exclude.code.list2, "[[", 1)],
            do.call(cbind, out))
    })

    out3 <- lapply(1:length(out2), function(i) {
        cm_dummy2long(out2[[i]])
    })
    nms <- rep(names(out2), sapply(out3, nrow))
    out3 <- do.call(rbind, out3)
    out3[, 4] <- nms

    newnames <- gsub("w\\.", "", names(exclude.code.list2))
    delete1 <- out3[, "code"] %in% newnames

    if (which.cm(x2long.obj) == "cmtime") {
        out3$Start <- sec2hms(out3$start)
        out3$End <- sec2hms(out3$end)
        out3 <- out3[, c(1:3, 5:6, 4)]
        class(out3) <- class(out3)[!grepl("vname_", class(out3))]
        class(out3) <- c(class(out3), paste0("vname_", rm.var))
    }

    if (is.null(rm.var)) {
        out3 <- out3[, -ncol(out3)]
        class(out3) <- class(out3)[!grepl("vname_", class(out3))]
    }

    cn <- colnames(x2long.obj)
    if (is.null(rm.var)) {
        if (which.cm(x2long.obj) == "cmrange") {
            cn <- colnames(x2long.obj)[1:3]
        } else {
            cn <- colnames(x2long.obj)[1:5]
        }
    }
    colnames(out3) <- cn
    class(out3) <- c(class(out3), which.cm(x2long.obj))
    out3
}
