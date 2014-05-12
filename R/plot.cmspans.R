#' Plots a cmspans object
#' 
#' Plots a cmspans object.
#' 
#' @param x The sums_cmspans object
#' @param plot.var A factor plotting variable (y axis).
#' @param facet.vars An optional single vector or list of 1 or 2 to facet by.
#' @param title An optional title.
#' @param \ldots Other arguments passed to \code{gantt_wrap}.
#' @method plot cmspans
#' @export
plot.cmspans <- function(x, plot.var = NULL, facet.vars = NULL, title = "Gantt Plot", ...) {
    class(x) <- class(x) [!class(x) %in% "cmdf2long"]
    if(is.null(plot.var)) {
        plot.var <- colnames(x)[1]
    }
    if (is.null(facet.vars)) {
        fv <- gsub("vname_", "", class(x)[grepl("vname_", class(x))])
        if (!identical(fv, character(0)) && !any(colnames(x) %in% fv)) {
            stop(paste0(
                "Defualt plot.var does not match any column names of cmspan object:\n\n",
                "Column names altered after `cmspans` object created.\n", 
                "Explicitly supply `facet.vars` to plot function.")) 
        }

        if (length(unique(x[, fv])) > 1) {
            facet.vars <- fv
        }
    }
    gantt_wrap(x, plot.var = plot.var, facet.vars =facet.vars, title = title, ...)
}
