#' Word Network Plot
#' 
#' A network plot of words.  Shows the interconnected and supporting use of 
#' words between textual units containing key terms.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default NULL generates one 
#' output for all text.  Also takes a single grouping variable or a list of 1 
#' or more grouping variables.  
#' @param target.words A named list of vectors of words whose length corresponds 
#' to \code{label.colors} (+1 length in cloud colors for non matched terms).
#' @param stopwords Words to exclude from the analysis (default is Top100Words).
#' @param label.cex The magnification to be used for network plot labels 
#' relative to the current setting of cex.  Default is .8.
#' @param log.labels logical.  If TRUE uses a proportional log label for more 
#' readable labels.  The formula is: \code{log(SUMS)/max(log(SUMS)))}. 
#' \code{label.size} adds more control over the label sizes. 
#' @param label.size An optional sizing constant to add to labels if log.labels 
#' is TRUE.
#' @param edge.curved  logical.  If TRUE edges will be curved rather than 
#' straight paths.
#' @param vertex.shape The shape of the vertices (see 
#' \code{\link[igraph]{igraph.vertex.shapes}} for more).
#' @param edge.color  A character vector of length one corresponding to the 
#' color of the plot edges.
#' @param label.colors A character vector of length one corresponding to the 
#' color of the labels.
#' @param layout  layout types supported by igraph.  See 
#' \code{\link[igraph]{layout}}.
#' @param title.name The title of the plot.
#' @param title.padj Adjustment for the network plot title. For strings 
#' parallel to the axes, padj = 0 means right or top alignment, and padj = 1 
#' means left or bottom alignment.
#' @param title.location On which side of the network plot (1=bottom, 2=left, 
#' 3=top, 4=right).
#' @param title.font The font family of the cloud title.
#' @param title.cex Character expansion factor for the title. NULL and NA are 
#' equivalent to 1.0.
#' @param title.color A character vector of length one corresponding to the 
#' color of the title.
#' @param legend A character vector of names corresponding to the number of 
#' vectors in \code{match.string}.
#' @param legend.cex Character expansion factor for the  network plot legend. 
#' NULL and NA are equivalent to 1.0. 
#' @param legend.location The x and y co-ordinates to be used to position the 
#' network plot legend.  The location may also be specified by setting x to a 
#' single keyword from the list \code{"bottomright"}, \code{"bottom"}, 
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, 
#' \code{"topright"}, \code{"right"} and \code{"center"}. This places the legend on 
#' the inside of the plot frame at the given location. 
#' @param plot logical.  If TRUE plots a network plot of the words.
#' @return Silently returns a list of igraph parameters.  Optionally, plots the 
#' output.
#' @seealso \code{\link[qdap]{word.network.plot}},
#' \code{\link[igraph]{graph.adjacency}}
#' @keywords network
#' @examples
#' \dontrun{
#' word.network.plot(text.var=DATA$state, grouping.var=DATA$person)
#' word.network.plot(text.var=DATA$state, grouping.var=list(DATA$sex, 
#' DATA$adult))
#' word.network.plot(text.var=DATA$state, grouping.var=DATA$person, 
#'     title.name = "TITLE", log.labels=TRUE)
#' word.network.plot(text.var=raj.act.1, grouping.var=raj.act.1$person, 
#'   stopwords = Top200Words)
#' }
word.network.plot <-                                                                  
function(text.var, grouping.var = NULL, target.words = NULL, stopwords = Top100Words, 
    label.cex = .8, label.size = .5, edge.curved = TRUE, vertex.shape = "circle",     
    edge.color = "gray70", label.colors = "black", layout = NULL,                     
    title.name = NULL, title.padj =  -4.5, title.location = 3, title.font = NULL,     
    title.cex = .8, log.labels = FALSE, title.color = "black",                        
    legend = NULL, legend.cex = .8, legend.location = c(-1.54, 1.41), plot = TRUE) {                                                  
    if (class(text.var) == "adjacency.matrix") { #actually takes an adjaceny matrix   
       adj.mat.object <- text.var[["adjacency"]]                                      
    } else {                                                                          
        z <- wfm(text.var = text.var, grouping.var = grouping.var,       
            stopwords = stopwords)                                                    
        adj.mat.object <- adjmat(t(z))[["adjacency"]]                                 
    }                                                                                 
    g <- graph.adjacency(adj.mat.object, weighted=TRUE, mode ='undirected')   
    g <- simplify(g)                                                          
    V(g)$label <- V(g)$name                                           
    V(g)$degree <- degree(g)                                          
    SUMS <- diag(adj.mat.object)                                                      
    if (!log.labels) {                                                                
        V(g)$label.cex <- label.cex                                           
    } else {                                                                          
        V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + label.size             
    }                                                                                 
    if (!is.null(target.words)) {                                                     
        nwc <- length(label.colors)                                                   
        COLORS <- text2color(words = V(g)$label, recode.words = target.words,         
            colors = label.colors)                                                    
        V(g)$label.color <- COLORS                                            
    } else {                                                                          
        V(g)$label.color <- label.colors                                      
    }                                                                                 
    V(g)$shape <- vertex.shape                                                
    E(g)$color <- edge.color                                                  
    if (is.null(layout)) {                                                            
        layout <- igraph::layout.fruchterman.reingold(g)                              
    }                                                                                 
    if (plot) {                                                                       
        if (dev.interactive()) dev.new()                                              
        plot.igraph(g, layout=layout, vertex.size=0, vertex.color="white",            
            edge.curved = edge.curved)                                                
        if (is.null(title.padj)){                                                     
            title.padj = -4.5                                                         
        }                                                                             
        if (is.null(title.location)){                                                 
            title.location = 3                                                        
        }                                                                             
        if (!is.null(title.name)) {                                                   
            mtext(text = title.name, side = title.location, padj = title.padj,        
            col = title.color, family = title.font, cex = title.cex)                  
        }                                                                             
        if (!is.null(legend)){                                                        
            par(mar = rep(0, 4), xpd = NA)                                            
            legend(x = legend.location[1], y = legend.location[2],                    
                cex = legend.cex, legend = legend,                                    
                fill = label.colors[1:length(legend)])                                
            par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)                                
        }                                                                             
    }                                                                                 
    invisible(g)                                                                      
}                                                                                     

