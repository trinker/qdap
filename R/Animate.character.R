#' Animate Character
#' 
#' \code{Animate.character} - Animate a \code{\link[base]{character}} object.  
#' Typically this function is useful in conjunction with other \code{Animate}
#' objects to create complex animations with accompanying text.
#' 
#' character Method for Animate
#' @param x A \code{\link[base]{character}} object.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param width The width to break text at if \code{type = "text"}.
#' @param coord The x/y coordinate to plot the text..
#' @param just The \code{hjust} and \code{vjust} values to use for the text.
#' @param size The size to print the text.  Can be a vector of length 1 or equal 
#' to the length of \code{x}.
#' @param color  The color to print the text.  Can be a vector of length 1 or equal 
#' to the length of \code{x}.
#' @param border.color The \code{panel.border} color (see\code{\link[ggplot2]{theme}}).
#' @param \ldots Other arguments passed to \code{\link[ggplot2]{annotate}}.
#' @importFrom qdapTools %l%
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @method Animate character
#' @examples
#' \dontrun{
#' Animate(DATA[["state"]])
#' Animate(DATA[["state"]], color="red")
#' Animate(DATA[["state"]], color=RColorBrewer::brewer.pal(11, "Set3"), size=10)
#' cls <- DATA[["person"]] %l% data.frame(levels(DATA[["person"]]), 
#'     RColorBrewer::brewer.pal(5, "Set3"))
#' Animate(DATA[["state"]], color=cls, size=10, width=30)
#' cls2 <- DATA[["sex"]] %l% data.frame(c("m", "f"),c("lightblue", "pink"))
#' Animate(DATA[["state"]], color=cls2, just=c(.5, .5), coord = c(.5, .5))
#' 
#' ## Print method
#' print(Animate(DATA[["state"]], color=cls2, just=c(.5, .5), coord = c(.5, .5)), 
#'     pause=.25)
#' Animate(DATA[["state"]], color=sample(colors(), nrow(DATA)), 
#'     size=sample(4:13, nrow(DATA), TRUE), width=30,  just=c(.5, .5), coord = c(.5, .5))
#' }
Animate.character <- function(x, wc.time = TRUE, time.constant = 2, 
    width = 65, coord = c(.0, .5), just = c(.0, .5), 
    size = 5, color = "black", border.color = NA, ...) {

    y <- NULL
    
    txt <- lapply(x, function(y){
            paste(strwrap(y, width), collapse="\n")
        }) %>% unlist

    theplot <- ggplot2::ggplot(data.frame(x=0:1, y=0:1), ggplot2::aes(x, x, y=y)) + 
        ggplot2::geom_blank() + ggplot2::theme_bw() +
        ggplot2::theme( 
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(color=border.color)
        ) + 
        ggplot2::ylab(NULL) + 
        ggplot2::xlab(NULL) 

    if (length(color) == 1) color <- rep(color, length(txt))
    if (length(color) != length(txt)) {
        warning("length of color != length of x; default to `color = \"black\"`")
        color <- rep("black", length(txt))
    }

    if (length(size) == 1) size <- rep(size, length(txt))
    if (length(size) != length(txt)) {
        warning("length of size != length of x; default to `size = 5`")
        size <- rep(5, length(txt))
    }

    ggplots <- lapply(seq_along(txt), function(i){
        theplot + ggplot2::annotate("text", x = coord[1], color = color[i], size = size[i],
            y = coord[2], label = txt[i], vjust = just[2], hjust = just[1], ...)
    })
    
    wrds <- wc(txt)

    timings <- round(exp(wrds/(max(wrds, na.rm=TRUE)/time.constant)))

    if(wc.time) {
        ggplots <- rep(ggplots, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object and end match the network Animate
    ggplots <- unlist(list(list(theplot), ggplots, 
        list(theplot)), recursive=FALSE)

    ## add class info
    class(ggplots) <- "animated_character"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "text"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- NULL
    ggplots
}


#' Prints an animated_character  Object
#' 
#' Prints an animated_character  object.
#' 
#' @param x The animated_character  object.
#' @param pause The length of time to pause between plots.
#' @param \ldots ignored.
#' @import igraph
#' @method print animated_character 
#' @export
print.animated_character <- function(x, pause = 0, ...){
    
    invisible(lapply(x, function(y) {
        print(y)
        if (pause > 0) Sys.sleep(pause)
    }))

}


#' Plots an animated_character  Object
#' 
#' Plots an animated_character  object.
#' 
#' @param x The animated_character  object.
#' @param \ldots Other arguments passed to \code{print.animated_character}.
#' @method plot animated_character 
#' @export
plot.animated_character  <- function(x, ...){ 

    print(x, ...)

}




