#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
ncutWORDS <- 
function(freqWORDSobj, cut.n = 15) {
    ncutWORDSa <- function(freqWORDS.DF, cut.ns) {
        if (nrow(freqWORDS.DF) > cut.ns) {
            subset(freqWORDS.DF, FREQ >= freqWORDS.DF[cut.ns, "FREQ"])
        } else {
            freqWORDS.DF
        }
    }
    if (is.data.frame(freqWORDSobj) & is.data.frame(freqWORDSobj)) {
        ncutWORDSa(freqWORDS.DF = freqWORDSobj, cut.ns = cut.n)
    } else {
        lapply(freqWORDSobj, function(x) ncutWORDSa(x, cut.ns = cut.n))
    }
} 