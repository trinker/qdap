#' Convert cm_combine.dummy Back to Long
#'
#' \code{cm_combine.dummy} back to long.
#'
#' @param cm_long2dummy_obj An object from cm_combine.dummy
#' @param rm.var Name of the repeated measures column.  Default is 
#' \code{"time"}.
#' @return Returns a dataframe with co-occurrences of provided code columns.
#' @seealso \code{\link[qdap]{cm_long2dummy}},
#' \code{\link[qdap]{cm_combine.dummy}}
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
#' (out1 <- cm_long2dummy(x))
#' 
#' (z <- cm_range2long(foo, foo2, v.name="time"))
#' out2 <- cm_long2dummy(z, "time")
#' lapply(out2, head)
#' cm_combine.dummy(out1, combine.code = list(AB=qcv(AA, BB)))
#' 
#' combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' A <- cm_combine.dummy(out2, combine.code = combines)
#' head(A, 10)
#' B <- cm_combine.dummy(out1, combine.code = combines)
#' head(B, 10)
#' 
#' cm_dummy2long(A)
#' cm_dummy2long(B)
#' plot(cm_dummy2long(A))
#' }
cm_dummy2long <-
function(cm_long2dummy_obj, rm.var = "time") {

    ## Grab the comment from cm_long2dummy_obj
    com <- gsub("l2d_", "", which.lcm(cm_long2dummy_obj))

    ## If the cm_long2dummy_obj isn't a list make it so and named 
    if (is.matrix(cm_long2dummy_obj) | is.data.frame(cm_long2dummy_obj)) {
        nms <- utils::tail(as.character(substitute(cm_long2dummy_obj)), 1)
        cm_long2dummy_obj <- list(cm_long2dummy_obj)
        names(cm_long2dummy_obj) <- nms
    }

    outs <- lapply(cm_long2dummy_obj, function(x) {
        out <- lapply(1:ncol(x), function(i) {
            dummy2span(x[, i])
        })
        data.frame(code = rep(colnames(x), sapply(out, nrow)),
            do.call(rbind, out), stringsAsFactors = FALSE)
    })

    DF <- data.frame(do.call(rbind, outs), 
        rmvar = rep(names(outs), sapply(outs, nrow)), row.names = NULL, stringsAsFactors = FALSE)

    colnames(DF)[4] <- rm.var
    class(DF) <- c("cmspans", com, paste0("vname_", rm.var), 
        class(DF))
    DF
}

## Helper functions with `cm_dummy2long`
dummy2span <- function(cl){
    runs <- rle(unname(cl))
    ones <- runs[["values"]] == 1
    cums <- cumsum(runs[["lengths"]])
    e <- cums[ones]
    s <- e - runs[["lengths"]][ones]
    data.frame(start = s, end = e)
}
