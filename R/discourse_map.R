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
#' ## Quickly add/remove a title
#' Title(x) <- "Act 1"
#' x
#' Title(x) <- NULL
#' x
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
#' 
#' ## Repeated Measures (BASIC EXAMPLE)
#' ##------------------------------
#' 
#' ## First merge data and map to discourse per act 
#' ## to separate networks
#' 
#' dat <- key_merge(raj, raj.demographics)
#' list_dat <- split(dat, dat$act)
#' plot_dat <- lapply(list_dat, function(x) with(x, discourse_map(dialogue, person)))
#' 
#' opar <- par()$mar
#' par(mfrow=c(3, 2), mar=c(0, 0, 3, 0))
#' 
#' lapply(seq_along(plot_dat), function(i){
#'     plot(plot_dat[[i]])
#'     mtext(paste("Act", names(plot_dat)[i]), side=3)
#' })
#' 
#' 
#' ## Repeated Measures (EXTENDED EXAMPLE)
#' ##------------------------------
#' fam_key <- data.frame(fam=unique(raj.demographics$fam.aff),
#'     cols=qcv(blue, grey10, red, orange), 
#'     stringsAsFactors = FALSE)
#' 
#' par(mfrow=c(3, 2), mar=c(0, 1, 3, 1))
#' lapply(seq_along(plot_dat), function(i){
#' 
#'     THE_PLOT <- visual(plot_dat[[i]])
#' 
#'     V(THE_PLOT)$sex <- V(THE_PLOT)$name %l% raj.demographics[, 1:2]
#'     V(THE_PLOT)$color <- ifelse(V(THE_PLOT)$sex=="f", "pink", "lightblue")
#'     V(THE_PLOT)$family <- V(THE_PLOT)$name %l+% raj.demographics[, c(1, 3)]
#'     V(THE_PLOT)$label.color <- lookup(V(THE_PLOT)$family, fam_key)
#' 
#'     plot(THE_PLOT, edge.curved=TRUE)
#'     mtext(paste("Act", names(plot_dat)[i]), side=3)
#' })
#' frame()
#' bords <- rep("black", 7)
#' bords[3] <- "white"
#' legend(.29, .95, c("Female", "Male", NA, as.character(fam_key[, 1])), 
#'     fill=c("pink", "lightblue", NA, fam_key[, 2]), border=bords, cex=1.5) 
#' 
#' ## Reset graphics margins
#' par(mar=opar)
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

    qsep <- "|-|qdap|-|"

    if (missing(edge.constant)) {
        edge.constant <- length(unique(grouping)) * 2.5
    }

    DF <- map_df1(grouping, text.var) 
    DF2 <- map_df2(DF, qsep)
    DF3 <- map_df3(DF)
    g <- map_graph_qdap(DF2, edge.constant)

    o <- list(raw = DF, edge_word_count=DF2, 
        vertex_word_count=DF3, plot = g)
    class(o) <- "discourse_map"
    o
}


map_df1 <- function(grouping, text.var){
    DF <- data.frame(from=grouping, wc=wc(text.var), 
        check.names = FALSE, stringsAsFactors = FALSE)
    DF[, "from"] <- factor(DF[, "from"])
    DF[, "to"] <- factor(c(as.character(DF[-1, "from"]), "End"))
    DF <- DF[, c(1, 3, 2)] 
    DF
}

map_df2 <- function(DF, qsep){
    DF2 <- colpaste2df(DF, 1:2, keep.orig=FALSE, sep=qsep, name.sep ="|")
    DF2 <- colsplit2df(list2df(lapply(split(DF2[, "wc"], 
        DF2[, "from|to"]) , sum), "wc", "from&to")[, 2:1], sep=qsep)
    DF2[, "prop_wc"] <- DF2["wc"]/sum(DF2[, "wc"])
   DF2
}


map_df3 <- function(DF){
    DF3 <- matrix2df(do.call(rbind, lapply(split(DF[, "wc"], 
        DF[, "from"]), sum)), "from")
    names(DF3)[2] <- "wc"
    DF3[, "prop_wc"] <- DF3["wc"]/sum(DF3[, "wc"])
    DF3
}


map_graph_qdap <- function(DF2, edgeconstant){
    g <- graph.data.frame(DF2, directed=TRUE)
    V(g)$size <- 10 
    E(g)$width <- edgeconstant*DF2[, "prop_wc"]
    g
}

#' Prints a discourse_map Object
#' 
#' Prints a discourse_map object.
#' 
#' @param x The discourse_map object.
#' @param edge.curved logical.  If \code{TRUE} edges are plotted with curves.
#' @param title The title of the plot.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @method print discourse_map
#' @S3method print discourse_map
print.discourse_map <- function(x, edge.curved = TRUE, title = NULL, ...) {
    plot.igraph(x[["plot"]], edge.curved = edge.curved, ...)
    if (!is.null(title)) {
        mtext(title, side=3)
    } else { 
        if (!is.null(attributes(x)[["title"]])){
            mtext(Title(x), side=3)
        }
    }
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
#' @param \ldots Other arguments passed to \code{print.discourse_map}.
#' @method plot discourse_map
#' @export
plot.discourse_map <- function(x, ...){ 

    print(x, ...)

}




