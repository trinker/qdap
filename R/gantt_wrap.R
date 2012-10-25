#' Gantt Plot Wrapper
#' 
#' A ggplot2 wrapper that produces a Gantt plot
#' 
#' @param dataframe a data frame with ploting variable(s) and a column of start and end times.
#' @param plot.var a factor plotting variable (y axis)
#' @param facet.vars an optional single vector or list of 1 or 2 to facet by
#' @param fill.var an optional variable to fill the code stips by.
#' @param title an optional title for the plot.
#' @param ylab an optional y label.
#' @param xlab an optional x label.
#' @param rev.factor logical.  if TRUE reverse the current plotting order so the first element in the plotting variable's levels is plotted on top.
#' @param transform logical.  if TRUE the repeated facets will be transformed from stacked to side by side.
#' @param minor.line.freq a numeric value for frequency of minor grid lines.
#' @param major.line.freq a numeric value for frequency of major grid lines.
#' @param sig.dig.line.freq An internal rounding factor.  Generally, default value surfices.
#' @param scale should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")
#' @param space if "fixed", the default, all panels have the same size. If "free_y" their height will be proportional to the length of the y scale; if "free_x" their width will be proportional to the length of the x scale; or if "free" both height and width will vary. This setting has no effect unless the appropriate scales also vary.
#' @param size the width of the plot bars.
#' @param rm.horiz.lines logical.  if TRUE the horzontal lines will be removed
#' @param x.ticks  logical.  if TRUE the x tixks will be displayed
#' @param y.ticks  logical.  if TRUE the y tixks will be displayed
#' @param legend.position the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
#' @param bar.color optional color to constrain all bars
#' @param border.color the color to plot border around Gantt bars (default is NULL)
#' @param border.size the size to plot borders around Gantt bars. Controls length (width also controlled if not specified).
#' @param border.width controls broder width around Gantt bars.  Use a numeric value in addition to border size if plot borders appear disproportional.
#' @return Returns a Gantt style visualization.
#' @author Andrie de Vries and and Tyler Rinker <tyler.rinker@gmail.com>.
#' @seealso 
#' \code{\link[qdap]{gantt}},
#' \code{\link[qdap]{gantt_plot}},
#' \code{\link[qdap]{gantt_rep}}
#' @references Wallace Clark and Henry Gantt (1922) The Gantt chart, a working tool of management. New York, Ronald Press
#' @keywords Gantt
#' @examples
#' (dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "sentences",                
#'      plot.colors = 'black', sums = TRUE, col.sep = "_")$gantt.df)     
#' gantt_wrap(dat, fam.aff_sex, title = "Gantt Plot")  
#' dat$codes <- sample(LETTERS[1:3], nrow(dat), TRUE)
#' gantt_wrap(dat, fam.aff_sex, fill.var = "codes", legend.position = "bottom")
#'
#' (dat3 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex), 
#'    units = "words", col.sep = "_")))    
#' x <- gantt_wrap(dat3, fam.aff_sex, facet.vars = "act", 
#'     title = "Repeated MeasuresGantt Plot")
#' x + scale_color_manual(values=rep("black", length(levels(dat3$fam.aff_sex)))) 
gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, fill.var = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.default", rev.factor = TRUE,
    transform = FALSE, minor.line.freq = NULL, major.line.freq = NULL, sig.dig.line.freq = -2,
    scale = NULL, space = NULL, size = 3, rm.horiz.lines = FALSE, x.ticks = TRUE, 
    y.ticks = TRUE, legend.position = NULL, bar.color = NULL, 
    border.color = NULL, border.size = 2, border.width = .1) { 
    plot.var2 <- as.character(substitute(plot.var))
    if(plot.var2 != "NAME") {
        plot.var <- as.character(substitute(plot.var))
    }
    if (rev.factor) {
        dataframe[, "new"] <- factor(dataframe[, plot.var], 
            levels=rev(levels(dataframe[, plot.var])))
    } else {
        dataframe[, "new"] <- factor(dataframe[, plot.var], 
            levels=levels(dataframe[, plot.var]))
    }
    if(xlab == "duration.defalut") {
        if (!is.null(comment(dataframe))) {
            xlab <- paste0("duration (", comment(dataframe), ")")
        } else {
            xlab <- "duration"
        }
    }
    if (!is.null(facet.vars)) { 
        dataframe[, "new2"] <- dataframe[, facet.vars[1]]
        if (length(facet.vars) == 2) {
            dataframe[, "new3"] <- dataframe[, facet.vars[2]]
        }
    } 
    if (!is.null(fill.var)) {
         dataframe[, "new4"] <- dataframe[, fill.var]
    } else {
    dataframe[, "new4"] <- dataframe[, "new"] 
    }
    if (rm.horiz.lines) {
        cond <- element_blank()
    } else {
        cond <- NULL
    }
    if (!is.null(border.color)) {
      ld <- length(dataframe$start)
      dataframe$startp <- c(dataframe$start[1], (dataframe$start[-1] - border.size[1]))
      dataframe$endp <- c((dataframe$end[-ld] + border.size[1]), dataframe$end[ld])
    }  
    theplot <- ggplot(dataframe, aes(colour=new4)) 
    if (!is.null(minor.line.freq)) {                 
        theplot <- theplot + geom_vline(xintercept = seq(0, 
           round(max(dataframe$end), sig.dig.line.freq[1]), 
           minor.line.freq), colour="gray92", size = .025) 
    }       
    if (!is.null(major.line.freq)) {                                                  
        theplot <- theplot + geom_vline(xintercept = seq(0, 
            round(max(dataframe$end), sig.dig.line.freq[length(sig.dig.line.freq)]),            
           major.line.freq), colour="gray50", size = .05)  
    } 
    FUN <- function(x) {if(x) {NULL} else {element_blank()}}
    axis.ticks.x <- FUN(x.ticks)
    axis.ticks.y <- FUN(y.ticks) 
    if (!is.null(border.color)) {
        if (length(border.size) == 1) {
            border.size[2] <- size + size*border.width
        }
        theplot <- theplot + geom_segment(aes(x=startp, xend=endp, y=new, 
            yend=new), colour = border.color, size=border.size[2], legend.position = "none")  
    }                                                 
    if (is.null(fill.var) & !is.null(bar.color)) {
        theplot <- theplot + 
          geom_segment(aes(x=start, xend=end, y=new, yend=new), color=bar.color, size=size)  
    } else {                                           
        theplot <- theplot + 
            geom_segment(aes(x=start, xend=end, y=new, yend=new), size=size) 
    }
    theplot <- theplot +  
        ylab(ylab) +    
        xlab(xlab) +                   
        theme_bw() +                                                                  
        scale_x_continuous(expand = c(0,0))+                                     
        theme(panel.background = element_rect(fill=NA, color="black"),       
           panel.grid.major.y = cond,
           panel.grid.minor.y = cond,
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.ticks.x = axis.ticks.x,
           axis.ticks.y = axis.ticks.y) +
        ggtitle(title)
    if(is.null(fill.var)) {
        theplot <- theplot + theme(legend.position = "none")
    }  
    if(!is.null(legend.position) && !is.null(fill.var)) {    
        theplot <- theplot + theme(legend.position = legend.position)   
    }
    if (!is.null(facet.vars)) { 
        if (length(facet.vars) == 1) {
            if (transform) {
                theplot <- theplot + facet_grid(.~new2, scales = scale, space = space)           
            } else {
                theplot <- theplot + facet_grid(new2~., scales = scale, space = space)
            }
        } else {
            theplot <- theplot + facet_grid(new2~new3, scales = scale, space = space)
        }
    }         
    print(theplot)
    invisible(theplot)
}