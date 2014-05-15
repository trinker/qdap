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

#' Prints a Network Object
#' 
#' Prints a Network object.
#' 
#' @param x The Network object.
#' @param title The title of the plot.  \code{NULL} eliminates title.  \code{NA}
#' uses title attribute of the Network object.
#' @param title.color The color of the title.
#' @param layout \pkg{igraph} \code{layout} to use.
#' @param seed The seed to use in plotting the graph.
#' @param legend The coordinates of the legend. See 
#' \code{\link[plotrix]{color.legend}} for more information.
#' @param legend.cex character expansion factor. \code{NULL} and \code{NA} are 
#' equivalent to 1.0. See \code{\link[graphics]{mtext}} for more information.
#' @param legend.text.color The legend text color.
#' @param legend.gradient A vector of ordered colors to use for the gradient 
#' fills in the network edges.
#' @param bg The color to be used for the background of the device region. See
#' \code{\link[graphics]{par}} for more information. 
#' @param vertex.color The font family to be used for vertex labels.
#' @param vertex.size The size of the vertex.
#' @param vertex.label.color The color of the labels.
#' @param vertex.label.cex The font size for vertex labels. 
#' @param edge.label.color The color for the edge labels.  Use \code{NA} to 
#' remove.
#' @param edge.label.cex The font size of the edge labels.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @importFrom plotrix color.legend
#' @importFrom qdapTools lookup
#' @note The output from \code{Network} is an \pkg{igraph} object and can be
#' altered and plotted directly using \pkg{igraph}.  The \pkg{qdap} \code{print}
#' method is offered as a quick approach to styling the figure.  For mor control
#' use \code{\link[igraph]{V}}, \code{\link[igraph]{E}}, and
#' \code{plot.igrapgh}.
#' @method print Network
#' @export
print.Network <- function(x, title = NA, title.color = "black",
    seed = sample(1:10000, 1), layout=layout.auto,  
    legend = c(-.5, -1.5, .5, -1.45), legend.cex=1, bg=NULL, 
    legend.text.color = "black", legend.gradient = NULL, 
    vertex.color = "grey80", vertex.size = 9,
    vertex.label.color = "grey40", vertex.label.cex = 1.1, 
    edge.label.color = "black", edge.label.cex = .9, ...){
    
    if (!is.null(title) && is.na(title)) {
        title <- attributes(x)[["title"]]
    }

    V(x)$color <- vertex.color
    V(x)$label.color  <- vertex.label.color    
    V(x)$size <- vertex.size
    V(x)$label.cex <- vertex.label.cex
    E(x)$label.color <- edge.label.color
    E(x)$label.cex <- edge.label.cex    

    if (is.null(legend.gradient) | is.null(attributes(x)[["n.color.breaks"]])) {
        legend.gradient <- attributes(x)[["legend.gradient"]]
    } else {
        ## set up color gradients
        colfunc <- colorRampPalette(legend.gradient)
        legend.gradient <- colfunc(attributes(x)[["n.color.breaks"]])
        E(x)$color <- lookup(E(x)$color, attributes(x)[["legend.gradient"]],
            legend.gradient)

    }
    
    set.seed(seed)
    if (is.null(bg)) {
        par(mar=c(5, 0, 2, 0))
    } else {
        par(mar=c(5, 0, 2, 0), bg = bg)
    }

    plot.igraph(x, edge.curved=TRUE, layout=layout, ...)

    if (!is.null(title)) {
       mtext(title, side=3, col = title.color)
    }
    
    if (!is.null(legend)) {
        color.legend(legend[1], legend[2], legend[3], legend[4], 
            attributes(x)[["legend.label"]], legend.gradient, 
                cex = legend.cex, col = legend.text.color)
    }
}

#' Plots a Network  Object
#' 
#' Plots a Network  object.
#' 
#' @param x The Network  object.
#' @param \ldots Other arguments passed to \code{print.Network }.
#' @method plot Network 
#' @export
plot.Network  <- function(x, ...){ 

    print(x, ...)

}


#' Add themes to a Network object.
#'
#' This operator allows you to add themes to a Network object.
#'
#' @param Network.obj A object of class \code{Network}.
#' @param x A component to add to \code{Network.obj}
#' @export
#' @rdname addNetwork
#' @method + Network
"+.Network" <- function(Network.obj, x) { 

    if (is.function(x)) x <- x()

    if(!is.null(attributes(x)[["title"]][["title"]]) && is.na(attributes(x)[["title"]][["title"]])) {
        attributes(x)[["title"]][["title"]] <- attributes(Network.obj)[["title"]]
    }

    if(length(attributes(x)[["legend.gradient"]]) == 1 && is.na(attributes(x)[["legend.gradient"]])) {
        attributes(x)[["legend.gradient"]] <- attributes(Network.obj)[["legend.gradient"]]
    }
    
    print.Network(Network.obj, 
        title = attributes(x)[["title"]][["title"]], 
        title.color = attributes(x)[["title.color"]],
        layout = attributes(x)[["layout"]],
        legend = attributes(x)[["legend"]],
        legend.gradient = attributes(x)[["legend.gradient"]],        
        legend.cex = attributes(x)[["legend.cex"]],
        bg = attributes(x)[["bg"]],
        legend.text.color = attributes(x)[["legend.text.color"]],
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
#' @param legend.text.color The text legend text color.
#' @param legend.gradient A vector of ordered colors to use for the gradient 
#' fills in the network edges.
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
#' @import igraph
#' @importFrom plotrix color.legend
#' @rdname qtheme
qtheme <- function(x = "generic", title, title.color, layout, legend, 
    legend.cex, legend.text.color, legend.gradient, bg, vertex.color, 
    vertex.size, vertex.label.color, vertex.label.cex, edge.label.color, 
    edge.label.cex){

    default_theme <- list(title.color = "black",
        layout=layout.auto, legend = c(-.5, -1.5, .5, -1.45), legend.cex=1, 
        bg=NULL, legend.text.color = "black", vertex.color = "grey80", 
        vertex.size = 9, vertex.label.color = "grey50", 
        vertex.label.cex = 1.1, edge.label.color = "black", edge.label.cex = .9)

    if(missing(title)) title <- NA
    if(missing(legend.gradient)) legend.gradient <- NA    
    if(missing(title.color)) title.color <- default_theme[["title.color"]]
    if(missing(layout)) layout <- default_theme[["layout"]]
    if(missing(legend)) legend <- default_theme[["legend"]]
    if(missing(legend.cex)) legend.cex <- default_theme[["legend.cex"]]
    if(missing(bg)) bg <- default_theme[["bg"]]
    if(missing(legend.text.color)) legend.text.color <- default_theme[["legend.text.color"]]
    if(missing(vertex.color)) vertex.color <- default_theme[["vertex.color"]]
    if(missing(vertex.size)) vertex.size <- default_theme[["vertex.size"]]
    if(missing(vertex.label.color)) vertex.label.color <- default_theme[["vertex.label.color"]]
    if(missing(vertex.label.cex)) vertex.label.cex <- default_theme[["vertex.label.cex"]]
    if(missing(edge.label.color)) edge.label.color <- default_theme[["edge.label.color"]]
    if(missing(edge.label.cex)) edge.label.cex <- default_theme[["edge.label.cex"]]

    pars <- list(x = x, title = title, title.color = title.color, layout = layout, 
        legend = legend, legend.cex = legend.cex, 
        legend.gradient = legend.gradient, bg = bg, 
        legend.text.color = legend.text.color, vertex.color = vertex.color, 
        vertex.size = vertex.size, vertex.label.color = vertex.label.color, 
        vertex.label.cex = vertex.label.cex, edge.label.color = edge.label.color, 
        edge.label.cex = edge.label.cex)

    function(x = pars[["x"]], title = pars[["title"]], title.color = pars[["title.color"]], 
        layout = pars[["layout"]], legend = pars[["legend"]], 
        legend.cex = pars[["legend.cex"]], 
        legend.gradient = pars[["legend.gradient"]], bg = pars[["bg"]], 
        legend.text.color = pars[["legend.text.color"]], 
        vertex.color = pars[["vertex.color"]], vertex.size = pars[["vertex.size"]],
        vertex.label.color = pars[["vertex.label.color"]], 
        vertex.label.cex = pars[["vertex.label.cex"]],
        edge.label.color = pars[["edge.label.color"]], 
        edge.label.cex = pars[["edge.label.cex"]], ...) {

        x <- "generic"
        attributes(x)[["title"]] <- list(title=title)
        attributes(x)[["title.color"]] <- title.color
        attributes(x)[["layout"]] <- layout
        attributes(x)[["legend"]] <- legend
        attributes(x)[["legend.cex"]] <- legend.cex
        attributes(x)[["bg"]] <- bg
        attributes(x)[["legend.text.color"]] <- legend.text.color         
        attributes(x)[["legend.gradient"]] <- legend.gradient       
        attributes(x)[["vertex.color"]] <- vertex.color
        attributes(x)[["vertex.size"]] <- vertex.size
        attributes(x)[["vertex.label.color"]] <- vertex.label.color
        attributes(x)[["vertex.label.cex"]] <- vertex.label.cex
        attributes(x)[["edge.label.color"]] <- edge.label.color
        attributes(x)[["edge.label.cex"]] <- edge.label.cex
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
#' @param \ldots Additional arguments supplied to \code{qtheme}.
#' @export
#' @import igraph
#' @importFrom plotrix color.legend
#' @rdname qtheme
theme_nightheat <- qtheme(x = "nightheat", title.color = "white", 
    bg = "black", legend.text.color = "white", 
    legend.gradient = c("blue", "white", "red"), 
    vertex.label.color = "grey70", 
    edge.label.color="yellow", vertex.size=10)

