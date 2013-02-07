#' Convert h:m:s to Seconds
#' 
#' Converts a vector of h:m:s to seconds.
#' 
#' @param x A vector of times in h:m:s.
#' @return Returns a vector of times in seconds.  Generally, this function 
#' is for internal use.
#' @keywords time, conversion
#' @seealso \code{\link[chron]{times}},
#' \code{\link[qdap]{sec2hms}}
#' @export
#' @examples 
#' \dontrun{
#' hms2sec(c("02:00:03", "04:03:01"))
#' hms2sec(sec2hms(c(222, 1234, 55)))
#' }
hms2sec <- 
function(x) {
    hms <- as.character(x)
    op <- FALSE
    if (length(hms) == 1) {
        hms <- c(hms, "00:00:00")
        op <- TRUE  
    }
    DF <- sapply(data.frame(do.call(rbind, strsplit(hms, ":"))), function(x){
        as.numeric(as.character(x))
    })
    out <- DF[, 1] * 3600 + DF[, 2] * 60 + DF[, 3]
    if (op) {
        out <- out[1]
    }
    out
}
