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
termco.d <-
function(text.var, grouping.var, match.string,
    ignore.case = FALSE, zero.replace = 0, output = "percent", 
    digits = 2){
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) x[length(x)])
        paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    x <- termco(text.var = text.var, match.string = match.string,
        grouping.var = grouping.var, ignore.case = ignore.case)
    names(x)[1] <- NAME
    y <- termco.p(tco = x, output = output, digits = digits)
    if (zero.replace != 0){
        x[, -c(1:2)] <- lapply(x[, -c(1:2)], 
            function(x) replacer(x, 0, zero.replace))
        y[, -c(1:2)] <- lapply(y[, -c(1:2)], 
            function(x) replacer(x, 0, zero.replace))
    }
    z <- termco.rnp(x, y)
    h <- paste(zero.replace, "(", zero.replace, ")", sep="")
    z[, -c(1:2)] <- lapply(z[, -c(1:2)], 
        function(x) replacer(x, h, zero.replace))
    o <- list(raw = x, prop = y, rnp = z)
    class(o) <- "termco_d"
    return(o)
}
