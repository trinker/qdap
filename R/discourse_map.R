#' Discourse Mapping
#' 
#' View the flow of discourse from social actors.
#' 
#' @param text.var The text variable or a  \code{"word_stats"} object (i.e., the 
#' output of a \code{word_stats} function).       
#' @param grouping.var The grouping variables.  Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.  
#' @param edge.constant A constant to multiple the edges by.  Defaults (if 
#' \code{missing}) to 2.5 times the number of social actors.
#' @param \ldots ignored
#' @param sep The separator character to use between grouping variables.
#' @return Returns a list:
#' \item{raw}{The dataframe with to and from columns (the edges) + word counts}
#' \item{edge_word_count}{A dataframe of edges and word counts + proportional 
#' word count}
#' \item{vertex_word_count}{A dataframe of vertices and word counts + 
#' proportional word count}
#' \item{plot}{An \pkg{igraph} object}
#' @export
#' @import igraph
#' @examples
#' \dontrun{
#' discourse_map(DATA$state, list(DATA$person, DATA$sex))
#' x <- with(mraja1, discourse_map(dialogue, person))
#' x
#' lview(x)
#' library(igraph)
#' plot(visual(x), edge.curved=FALSE)
#' 
#' ## Augmenting the plot
#' mygraph <- visual(x)
#' 
#' plot(mygraph, edge.curved=TRUE)
#' 
#' V(mygraph)$sex <- V(mygraph)$name %l% raj.demographics[, 1:2]
#' V(mygraph)$color <- ifelse(V(mygraph)$sex=="f", "pink", "lightblue")
#' 
#' plot(mygraph, edge.curved=TRUE)
#' 
#' V(mygraph)$family <- V(mygraph)$name %l+% raj.demographics[, c(1, 3)]
#' cols <- qcv(blue, red, brown, darkgreen, grey10)
#' V(mygraph)$label.color <- lookup(V(mygraph)$family, 
#'     unique(V(mygraph)$family), cols)
#' 
#' plot(mygraph, edge.curved=TRUE)
#' }
discourse_map <- function(text.var, grouping.var, edge.constant, sep = "_", 
    ...) {

    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
        )
        G <- paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        G <- G[length(G)]
    }


    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping <- paste2(grouping.var, sep = sep, ...)
    } else {
        grouping <- unlist(grouping.var)
    } 

    DF <- data.frame(from=grouping, wc=wc(text.var), 
        check.names = FALSE, stringsAsFactors = FALSE)
    DF[, "from"] <- factor(DF[, "from"])
    DF[, "to"] <- factor(c(as.character(DF[-1, "from"]), "End"))
    DF <- DF[, c(1, 3, 2)] 

    qsep <- "|-|qdap|-|"

    DF2 <- colpaste2df(DF, 1:2, keep.orig=FALSE, sep=qsep, name.sep ="|")
    DF2 <- colsplit2df(list2df(lapply(split(DF2[, "wc"], 
        DF2[, "from|to"]) , sum), "wc", "from&to")[, 2:1], sep=qsep)
    DF2[, "prop_wc"] <- DF2["wc"]/sum(DF2[, "wc"])

    DF3 <- matrix2df(do.call(rbind, lapply(split(DF[, "wc"], 
        DF[, "from"]), sum)), "from")
    names(DF3)[2] <- "wc"
    DF3[, "prop_wc"] <- DF3["wc"]/sum(DF3[, "wc"])

    g <- graph.data.frame(DF2, directed=TRUE)
    V(g)$size <- 10 

    if (missing(edge.constant)) {
        edge.constant <- nrow(DF3) * 2.5
    }

    E(g)$width <- edge.constant*DF2[, "prop_wc"]

    o <- list(raw = DF, edge_word_count=DF2, 
        vertex_word_count=DF3, plot = g)
    class(o) <- "discourse_map"
    o
}

#' Prints a discourse_map Object
#' 
#' Prints a discourse_map object.
#' 
#' @param x The discourse_map object.
#' @param edge.curved logical.  If \code{TRUE} edges are plotted with curves.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @method print discourse_map
#' @S3method print discourse_map
print.discourse_map <- function(x, edge.curved = TRUE, ...) {
    plot.igraph(x[["plot"]], edge.curved = edge.curved, ...)
}


#'Discourse Map
#' 
#' \code{visual.discourse_map} - View visual from \code{\link[qdap]{discourse_map}}.
#' 
#' discourse_map Method for visual
#' @param x The discourse_map object.
#' @param \ldots ignored
#' @export
#' @method visual discourse_map
visual.discourse_map <- function(x, ...) {
    x[["plot"]]
}


#' Plots a discourse_map Object
#' 
#' Plots a discourse_map object.
#' 
#' @param x The discourse_map object.
#' @param \ldots ignored
#' @method plot discourse_map
#' @export
plot.discourse_map <- function(x, ...){ 

    print(x)

}




