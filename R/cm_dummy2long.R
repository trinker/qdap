#' Convert cm_combine.dummy Back to Long
#'
#' \code{cm_combine.dummy} back to long.
#'
#' @param cm.comb.obj An object from cm_combine.dummy
#' @param rm.var Name of the repeated measures column.  Default is \code{"time".}
#' @return Returns a dataframe with co-occurrences of provided code columns.
#' @seealso \code{\link[qdap]{cm_long2dummy}},
#' \code{\link[qdap]{cm_combine.dummy}}
#' @export
#' @examples
#' \dontrun{
#' foo <- list(
#'     AA = qcv(terms='1:10'),
#'     BB = qcv(terms='1:2, 3:10, 19'),
#'     CC = qcv(terms='1:3, 5:6')
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms='4:8'),
#'     BB = qcv(terms='10:12'),
#'     CC = qcv(terms='1, 11, 15:20'),
#'     DD = qcv(terms='')
#' )
#' 
#' x <- cm_range2long(foo)
#' D1 <- cm_long2dummy(x)
#' 
#' z <- cm_range2long(foo, foo2, v.name="time")
#' D2 <- cm_long2dummy(z, "time")
#' cm_combine.dummy(D1, combine.code = list(AB=qcv(AA, BB)))
#' 
#' combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' 
#' A <- cm_combine.dummy(D2, combine.code = combines)
#' B <- cm_combine.dummy(D1, combine.code = combines)
#' 
#' cm_dummy2long(A)
#' cm_dummy2long(B, "time")
#' }
cm_dummy2long <- function(cm.comb.obj, rm.var = "time") {
    L1 <- split(cm.comb.obj[, !colnames(cm.comb.obj) %in% rm.var], 
         cm.comb.obj[, rm.var])
    lng <- function(x) {
        a <- rle(x)
        lens <- a[[1]]
        ends <- cumsum(lens)[as.logical(a[[2]])]
        starts <- ends - lens[(as.logical(a[[2]]))]
        data.frame(start=starts, end=ends)
    }
    spanner <- function(A) {lapply(A, lng)}
    L2 <- lapply(L1, spanner)
    invisible(lapply(seq_along(L2), function(i) {
        tnms <- names(L2)[i]
        cnms <- names(L2[[i]])
        invisible(lapply(seq_along(L2[[i]]), function(j) {
            if (nrow(L2[[i]][[j]]) == 0) {
                return()
            }
            L2[[i]][[j]][, "code"] <<- cnms[j] 
            L2[[i]][[j]][, rm.var] <<- names(L2)[i]
        }))     
    }))
    L2 <- unlist(L2, recursive = FALSE)
    L2 <- L2[sapply(L2, function(x) nrow(x) != 0)]
    dat <- data.frame(do.call(rbind, L2), row.names=NULL)
    data.frame(dat[, 3, drop=FALSE], dat[, -3, drop=FALSE])
}

