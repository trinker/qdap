#' Generic Network Method
#' 
#' Create a network plt for select qdap outputs.
#' 
#' @param x A select qdap object.
#' @param \ldots Arguments passed to Network method of other classes.
#' @export
#' @rdname Network
#' @return Returns a network plot.
Network <-
function(x, ...){
    UseMethod("Network")
}


#' Add themes to a Network object.
#'
#' This operator allows you to add themes to a Network object.
#'
#' @param Network.obj A object of class \code{Network}.
#' @param x A component to add to \code{Network.obj}
#'
#' @export
#' @rdname Network
#' @method + Network
"+.Network" <- function(Network.obj, x) { 

    if (is.function(x)) x <- x()

    if(!is.null(attributes(x)[["title"]][["title"]]) && is.na(attributes(x)[["title"]][["title"]])) {
        attributes(x)[["title"]][["title"]] <- attributes(Network.obj)[["title"]]
    }

    print.Network(Network.obj, 
        title = attributes(x)[["title"]][["title"]], 
        title.color = attributes(x)[["title.color"]],
        layout = attributes(x)[["layout"]],
        legend = attributes(x)[["legend"]],
        legend.cex = attributes(x)[["legend.cex"]],
        bg = attributes(x)[["bg"]],
        legend.color = attributes(x)[["legend.color"]],
        vertex.color = attributes(x)[["vertex.color"]],
        vertex.size = attributes(x)[["vertex.size"]],
        vertex.label.color = attributes(x)[["vertex.label.color"]],
        vertex.label.cex = attributes(x)[["vertex.label.cex"]],
        edge.label.color = attributes(x)[["edge.label.color"]],
        edge.label.cex = attributes(x)[["edge.label.cex"]],
    )
}


#' Add themes to a Network object.
#'
#' This function builds generic themes to add a theme to a Network object rather 
#' than individual \code{print} arguments.
#' 
#' @param x The name of the qtheme.
#' @param title.color The color of the title.
#' @param layout \pkg{igraph} \code{layout} to use.
#' @param legend The coordinates of the legend. See 
#' \code{\link[plotrix]{color.legend}} for more information.
#' @param legend.cex character expansion factor. \code{NULL} and \code{NA} are 
#' equivalent to 1.0. See \code{\link[graphics]{mtext}} for more information.
#' @param legend.color The text legend color for the network plot.
#' @param bg The color to be used for the background of the device region. See
#' \code{\link[graphics]{par}} for more information. 
#' @param vertex.color The font family to be used for vertex labels.
#' @param vertex.size The size of the vertex.
#' @param vertex.label.color The color of the labels.
#' @param vertex.label.cex The font size for vertex labels. 
#' @param edge.label.color The color for the edge labels.  Use \code{NA} to 
#' remove.
#' @param edge.label.cex The font size of the edge labels.
#' @export
#' @rdname theme
qtheme <- function(x = "generic", title, title.color, layout, legend, 
    legend.cex, legend.color, bg, vertex.color, vertex.size,
    vertex.label.color, vertex.label.cex, edge.label.color, 
    edge.label.cex){

    default_theme <- list(title.color = "black",
        layout=layout.auto, legend = c(-.5, -1.5, .5, -1.45), legend.cex=1, 
        bg=NULL, legend.color = "black", vertex.color = "grey40", 
        vertex.size = 10, vertex.label.color = "deepskyblue", 
        vertex.label.cex = 1.1, edge.label.color = "black", edge.label.cex = .9)

    if(missing(title)) title <- NA
    if(missing(title.color)) title.color <- default_theme[["title.color"]]
    if(missing(layout)) layout <- default_theme[["layout"]]
    if(missing(legend)) legend <- default_theme[["legend"]]
    if(missing(legend.cex)) legend.cex <- default_theme[["legend.cex"]]
    if(missing(bg)) bg <- default_theme[["bg"]]
    if(missing(legend.color)) legend.color <- default_theme[["legend.color"]]
    if(missing(vertex.color)) vertex.color <- default_theme[["vertex.color"]]
    if(missing(vertex.size)) vertex.size <- default_theme[["vertex.size"]]
    if(missing(vertex.label.color)) vertex.label.color <- default_theme[["vertex.label.color"]]
    if(missing(vertex.label.cex)) vertex.label.cex <- default_theme[["vertex.label.cex"]]
    if(missing(edge.label.color)) edge.label.color <- default_theme[["edge.label.color"]]
    if(missing(edge.label.cex)) edge.label.cex <- default_theme[["edge.label.cex"]]

    pars <- list(x = x, title = title, title.color = title.color, layout = layout, 
       legend = legend, legend.cex = legend.cex, bg = bg, 
       legend.color = legend.color, vertex.color = vertex.color, 
       vertex.size = vertex.size, vertex.label.color = vertex.label.color, 
       vertex.label.cex = vertex.label.cex, edge.label.color = edge.label.color, 
       edge.label.cex = edge.label.cex)

    function(x = pars[["x"]], title = pars[["title"]], title.color = pars[["title.color"]], 
        bg = pars[["bg"]], legend.color = pars[["legend.color"]], 
        vertex.color = pars[["vertex.color"]], vertex.size = pars[["vertex.size"]],
        vertex.label.color = pars[["vertex.label.color"]], 
        edge.label.color = pars[["edge.label.color"]], ...) {

        x <- "generic"
        attributes(x)[["title"]] <- list(title=title)
        attributes(x)[["title.color"]] <- title.color
        attributes(x)$layout <- layout
        attributes(x)$legend <- legend
        attributes(x)$legend.cex <- legend.cex
        attributes(x)$bg <- bg
        attributes(x)$legend.color <- legend.color
        attributes(x)$vertex.color <- vertex.color
        attributes(x)$vertex.size <- vertex.size
        attributes(x)$vertex.label.color <- vertex.label.color
        attributes(x)$vertex.label.cex <- vertex.label.cex
        attributes(x)$edge.label.color <- edge.label.color
        attributes(x)$edge.label.cex <- edge.label.cex
        x
    }
}



#' Add themes to a Network object.
#'
#' This theme allows you to add a night heat theme to a Network object rather 
#' than individual \code{print} arguments.
#' 
#' @param title The title of the plot.  \code{NULL} eliminates title.  \code{NA}
#' uses title attribute of the Network object.
#' @export
#' @rdname theme
qtheme_nightheat <- qtheme(x = "nightheat", title.color = "white", 
    bg = "black", legend.color = "white", vertex.label.color = "grey70", 
    edge.label.color="yellow", vertex.size=10)


