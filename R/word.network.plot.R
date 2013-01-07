#' A network plot of words
#' 
#' A network plot of words
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param target.words %% ~~Describe \code{target.words} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
#' @param label.cex %% ~~Describe \code{label.cex} here~~
#' @param label.size %% ~~Describe \code{label.size} here~~
#' @param edge.curved %% ~~Describe \code{edge.curved} here~~
#' @param vertex.shape %% ~~Describe \code{vertex.shape} here~~
#' @param edge.color %% ~~Describe \code{edge.color} here~~
#' @param label.colors %% ~~Describe \code{label.colors} here~~
#' @param layout %% ~~Describe \code{layout} here~~
#' @param title.name %% ~~Describe \code{title.name} here~~
#' @param title.padj %% ~~Describe \code{title.padj} here~~
#' @param title.location %% ~~Describe \code{title.location} here~~
#' @param title.font %% ~~Describe \code{title.font} here~~
#' @param title.cex %% ~~Describe \code{title.cex} here~~
#' @param log.labels %% ~~Describe \code{log.labels} here~~
#' @param title.color %% ~~Describe \code{title.color} here~~
#' @param plot %% ~~Describe \code{plot} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
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

