#' Convert Seconds to h:m:s
#' 
#' Converts a vector of seconds to h:m:s
#' 
#' @param x
#' @param l1
#' @return Returns a vector of times in h:m:s format.  Generally, this function 
#' is for internal use.
#' @keywords time, coversion
#' @export
#' @examples 
#' convert(c(256, 3456, 56565))
#' convert(c(256, 3456, 56565), TRUE)
convert <-
function(x, l1 = FALSE) {
    if (length(x) == 1) {
        x <- c(x, 0)
        l1 <- TRUE
    }
    h <- floor(x/3600)
    m <- floor((x-h*3600)/60)
    s <- x-(m*60 + h*3600)
    pad <- function(x) sprintf("%02d", as.numeric(x))
    op <- times(paste2(data.frame(apply(data.frame(h=h, m=m, s=s), 2, pad)), sep=":"))
    if (l1) {
        op[1]
    } else {
        op
    }
}
