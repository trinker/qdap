#' @param text.var
#' @param grouping.var
#' @param stopwords
#' @param rm.duplicates
#' @param title
#' @param title.font
#' @param title.color
#' @param title.cex
#' @param title.name
#' @param legend
#' @param legend.cex
#' @param legend.location
#' @param legend.text.col
#' @param legend.horiz
#' @param \ldots
trans.venn <-
function(text.var, grouping.var, stopwords = NULL, 
    rm.duplicates = TRUE, title = TRUE, title.font = NULL, 
    title.color = "black", title.cex = NULL, title.name = NULL, 
    legend = TRUE,   legend.cex = .8, legend.location = "bottomleft", 
    legend.text.col = "black", legend.horiz = FALSE, ...) {
    x <- wfm(text.var, grouping.var, stopwords = stopwords)
    if (!rm.duplicates) {
        Counts <- wfm.expanded(x)
    } else {
        Counts <- apply(x, 2, function(x) as.numeric(x>0))
    }
    v <- venneuler(Counts)
    if (title) {
        if (!is.null(title.name)){
            title.name <- title.name
        } else {
            title.name <- "Venn Diagram"
        }
    } else {
        title.name <- ""
    }
    plot(v, main = title.name, cex.main = title.cex, col.main = title.color,
        family = title.font, ...)
    if (legend) {
        cols <- col.fn(v$colors)
        par(mar = rep(0, 4), xpd = TRUE)
        legend(legend.location[1], legend.location[2], horiz = legend.horiz,
            legend = colnames(Counts), fill = cols, cex = legend.cex, 
            text.col = legend.text.col)
        par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
    }
    invisible(list(freq.mat = Counts, venneuler.obj = v))
}