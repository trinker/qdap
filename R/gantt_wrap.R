#' Gantt Plot
#' 
#' A ggplot2 wrapper that produces a Gantt plot
#' 
#' @param dataframe A data frame with plotting variable(s) and a column of start 
#' and end times.
#' @param plot.var A factor plotting variable (y axis)
#' @param facet.vars An optional single vector or list of 1 or 2 to facet by
#' @param fill.var An optional variable to fill the code strips by.
#' @param title An optional title for the plot.
#' @param ylab An optional y label.
#' @param xlab An optional x label.
#' @param rev.factor logical.  If TRUE reverse the current plotting order so the 
#' first element in the plotting variable's levels is plotted on top.
#' @param ncol if an integer value is passed to this gantt_wrap uses facet_wrap 
#' rather than facet_grid
#' @param transform logical.  If TRUE the repeated facets will be transformed 
#' from stacked to side by side.
#' @param minor.line.freq A numeric value for frequency of minor grid lines.
#' @param major.line.freq A numeric value for frequency of major grid lines.
#' @param sig.dig.line.freq An internal rounding factor for minor and major line 
#' freq.  Generally, default value of 1 suffices for larger range of x scale may 
#' need to be set to -2..
#' @param hms.scale logical.  If TRUE converts scale to h:m:s format.  Default 
#' NULL attempts to detect if object is a cm_time2long object
#' @param scale Should scales be fixed ("fixed", the default), free ("free"), or 
#' free in one dimension ("free_x", "free_y")
#' @param space If "fixed", the default, all panels have the same size. If 
#' "free_y" their height will be proportional to the length of the y scale; if 
#' "free_x" their width will be proportional to the length of the x scale; or if 
#' "free" both height and width will vary. This setting has no effect unless the 
#' appropriate scales also vary.
#' @param size The width of the plot bars.
#' @param rm.horiz.lines logical.  If TRUE the horizontal lines will be removed.
#' @param x.ticks  logical.  If TRUE the x ticks will be displayed.
#' @param y.ticks  logical.  If TRUE the y ticks will be displayed.
#' @param legend.position The position of legends. ("left", "right", "bottom", 
#' "top", or two-element numeric vector).
#' @param bar.color Optional color to constrain all bars.
#' @param border.color The color to plot border around Gantt bars (default is 
#' NULL).
#' @param border.size An integer value for the size to plot borders around Gantt 
#' bars. Controls length (width also controlled if not specified).
#' @param border.width Controls border width around Gantt bars.  Use a numeric 
#' value in addition to border size if plot borders appear disproportional.
#' @param constrain logical.  If TRUE the Gantt bars touch the edge of the graph.
#' @return Returns a Gantt style visualization. Invisibly returns the ggplot2 
#' list object.
#' @note For non repeated measures data/plotting use \code{\link[qdap]{gantt}}; 
#' for repeated measures data output use \code{\link[qdap]{gantt_rep}}; and for 
#' a convenient wrapper that takes text and generates plots use 
#' \code{\link[qdap]{gantt_plot}}.
#' @import ggplot2 scales RColorBrewer
#' @author Andrie de Vries and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso 
#' \code{\link[qdap]{gantt}},
#' \code{\link[qdap]{gantt_plot}},
#' \code{\link[qdap]{gantt_rep}},
#' \code{\link[ggplot2]{facet_grid}},
#' \code{\link[ggplot2]{facet_wrap}}
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working tool of 
#' management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @examples
#' \dontrun{
#' dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), 
#'     units = "sentences", plot.colors = 'black', sums = TRUE, 
#'     col.sep = "_")$gantt.df
#' htruncdf(dat)     
#' gantt_wrap(dat, fam.aff_sex, title = "Gantt Plot")  
#' dat$codes <- sample(LETTERS[1:3], nrow(dat), TRUE)
#' gantt_wrap(dat, fam.aff_sex, fill.var = "codes", legend.position = "bottom")
#'
#' dat2 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex), 
#'     units = "words", col.sep = "_"))
#' htruncdf(dat2)  
#' x <- gantt_wrap(dat2, fam.aff_sex, facet.vars = "act", 
#'     title = "Repeated Measures Gantt Plot")
#'     
#' library(ggplot2); library(scales); library(RColorBrewer)
#' x + scale_color_manual(values=rep("black", length(levels(dat2$fam.aff_sex)))) 
#' }
gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, fill.var = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.default", rev.factor = TRUE,
    transform = FALSE, ncol = NULL, minor.line.freq = NULL, 
    major.line.freq = NULL, sig.dig.line.freq = 1, hms.scale = NULL, 
    scale = NULL, space = NULL, size = 3, rm.horiz.lines = FALSE, x.ticks = TRUE, 
    y.ticks = TRUE, legend.position = NULL, bar.color = NULL,
    border.color = NULL, border.size = 2, border.width = .1, constrain = TRUE) { 
    new4 <- startp <- endp <- NULL
    if (is.null(hms.scale)) {
        if (!is.null(comment(dataframe)) && comment(dataframe) == "cmtime") {
            hms.scale <- TRUE
        } else {
            hms.scale <- FALSE
            if(!is.null(comment(dataframe))) {
                if (comment(dataframe) == "cmrange" & xlab == "duration.default"){
                    xlab <- "Duration (words)"
                }
            }
        }
    }
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
    if(xlab == "duration.default") {
        if (hms.scale) {
                xlab <- "Duration (hours:minutes)"
        } else {
            if (!is.null(comment(dataframe))) {
                xlab <- paste0("Duration (", comment(dataframe), ")")
            } else {
                xlab <- "Duration"
            }
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
        dataframe$startp <- c((dataframe$start - border.size[1]))
        dataframe$endp <- c((dataframe$end + border.size[1]))
        if (hms.scale) {
            dataframe$startp[dataframe$startp <= 0] <- 0
            dataframe$startp <- as.numeric(convert(dataframe$startp +5))- 
                as.numeric(convert(border.size[1]))
            dataframe$endp <- as.numeric(convert(dataframe$endp))
        }
    } 
    if (hms.scale) {
        if (all(colnames(dataframe) %in% c("Start", "End"))) {
            dataframe$start <- dataframe$Start
            dataframe$end <- dataframe$End
        } else {
            dataframe$start <- convert(dataframe$start)
            dataframe$end <- convert(dataframe$end)
        }
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
            yend=new), colour = border.color, size=border.size[2], 
            legend.position = "none")  
    }                                                 
    if (is.null(fill.var) & !is.null(bar.color)) {
        theplot <- theplot + 
            geom_segment(aes(x=start, xend=end, y=new, yend=new), 
                color=bar.color, size=size)  
    } else {                                           
        theplot <- theplot + 
            geom_segment(aes(x=start, xend=end, y=new, yend=new), size=size) 
    }
    theplot <- theplot +  
        ylab(ylab) +    
        xlab(xlab) +                   
        theme_bw()  
    if (hms.scale) {
        times_trans <- function() {
            fmt <- function(x) {
                format(x, simplify = !any(diff(x) < 1/(24*60)))
            }
            trans_new("chrontimes",
                      transform = as.numeric,
                      inverse = times,
                      breaks = pretty_breaks(),
                      format = fmt,
                      domain=c(0,1))
        }
        if (constrain) {
            theplot <- theplot + scale_x_continuous(expand = c(0, 0), 
                trans=times_trans())
        } else {
            theplot <- theplot + scale_x_continuous(trans=times_trans())
        }  
    } else {                                                       
        if (constrain) {
            theplot <- theplot + scale_x_continuous(expand = c(0, 0))
        } 
    }  
    theplot <- theplot +                             
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
        if(!is.null(ncol)){
            theplot <- theplot + facet_wrap(~new2, scales = scale, ncol=ncol)           
        } else {
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
    }    
    if (!is.null(fill.var)){
        theplot <- theplot + guides(colour = guide_legend(fill.var))
    }
    print(theplot)
    invisible(theplot)
}