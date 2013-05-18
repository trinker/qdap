#' Venn Diagram by Grouping Variable
#' 
#' Produce a Venn diagram by grouping variable.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default NULL generates one 
#' output for all text.  Also takes a single grouping variable or a list of 1 
#' or more grouping variables.  
#' @param stopwords Words to exclude from the analysis.
#' @param rm.duplicates logical.  IF TRUE removes the duplicated words from the 
#' analysis (only single usage is considered).
#' @param title logical.  IF TRUE adds a title corresponding to the 
#' \code{grouping.var}.
#' @param title.font The font family of the cloud title. 
#' @param title.color A character vector of length one corresponding to the 
#' color of the title.
#' @param title.cex Character expansion factor for the title. NULL and NA are 
#' equivalent to 1.0
#' @param title.name A title for the plot.
#' @param legend logical.  If TRUE uses the names from the \code{target.words}
#' list corresponding to cloud.colors. 
#' @param legend.cex Character expansion factor for the legend. NULL and NA are 
#' equivalent to 1.0. 
#' @param legend.location The x and y co-ordinates to be used to position the 
#' legend.  The location may also be specified by setting x to a 
#' single keyword from the list \code{"bottomright"}, \code{"bottom"}, 
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, 
#' \code{"topright"}, \code{"right"} and \code{"center"}. This places the legend on 
#' the inside of the plot frame at the given location. 
#' @param legend.text.col The color used for the legend text.
#' @param legend.horiz logical; if TRUE, set the legend horizontally rather than 
#' vertically.
#' @param \dots Other arguments passed to plot.
#' @return Returns a Venn plot by grouping variable(s).
#' @section Warning: The algorithm used to overlap the Venn circles becomes 
#' increasingly overburdened and less accurate with increased grouping 
#' variables. An alternative is to use a network plot with dissimilarity 
#' measures labeling the edges between nodes (grouping variables).
#' @seealso \code{\link[venneuler]{venneuler}}
#' @keywords venn
#' @export
#' @import venneuler gdata
#' @examples
#' \dontrun{
#' with(DATA , trans.venn(state, person, legend.location = "topright"))
#' #the plot below will take a considerable amount of time to plot
#' with(raj.act.1 , trans.venn(dialogue, person, legend.location = "topleft"))
#' }
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
