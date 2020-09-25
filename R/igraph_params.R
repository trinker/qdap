#' Apply Parameter to List of Igraph Vertices/Edges
#' 
#' \code{vertex_apply} - Uniformly apply \pkg{igraph} vertex plotting parameters to a list of \pkg{igraph} objects.
#' 
#' @param x A list of \pkg{igraph} objects.
#' @param \ldots Arguments passed \pkg{igraph}'s \code{\link[igraph]{V}} and \code{\link[igraph]{E}}.  
#' See https://igraph.org/redirect.html
#' for more.
#' @param hold.ends A vector of parameters passed to \dots that should not be 
#' altered for the first and last (ends) objects in the list.
#' @return Returns a list of igraph objects.
#' @export
#' @rdname igraph_params
#' @import igraph
#' @examples
#' \dontrun{
#' x <- with(DATA.SPLIT, polarity(state, person))
#' bg_black <- Animate(x, neutral="white")
#' print(bg_black)
#' 
#' bgb <- vertex_apply(bg_black, label.color="grey80", size=20, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' print(bgb, bg="black", pause=.75)
#' }
vertex_apply <- function(x, ..., hold.ends=NULL) {
    
    args <- list(...)
    lims <- c(1, length(x))

    out <- lapply(seq_along(x), function(i){   
        inds <- seq_along(args)
        for(j in inds) {    
            if (!i %in% lims | is.null(hold.ends) | 
                !names(args)[j] %in% hold.ends) {
                vertex.attributes(x[[i]])[[names(args)[j]]] <- rep(args[[j]], 
                    length(V(x[[i]])))
            }     
        }
        return(x[[i]])
    })

    class(out) <- class(x)
    attributes(out) <- attributes(x)
    out
}


#' Apply Parameter to List of Igraph Vertices/Edges
#' 
#' \code{edge_apply} - Uniformly apply \pkg{igrph} edge plotting parameters to a list of \pkg{igraph} objects.
#' 
#' @export
#' @rdname igraph_params
#' @import igraph
edge_apply <- function(x, ..., hold.ends=c("label.color")) {
    
    args <- list(...)
    lims <- c(1, length(x))

    out <- lapply(seq_along(x), function(i){   
        inds <- seq_along(args)
        for(j in inds) {   
            if (!i %in% lims | is.null(hold.ends) | 
                !names(args)[j] %in% hold.ends) {
                edge.attributes(x[[i]])[[names(args)[j]]] <- rep(args[[j]], 
                    length(E(x[[i]])))
            }     
        }
        return(x[[i]])
    })
    class(out) <- class(x)
    attributes(out) <- attributes(x)
    out
}

