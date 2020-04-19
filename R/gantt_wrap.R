#' Gantt Plot
#' 
#' A ggplot2 wrapper that produces a Gantt plot.
#' 
#' @param dataframe A data frame with plotting variable(s) and a column of start 
#' and end times.
#' @param plot.var A factor plotting variable (y axis).
#' @param facet.vars An optional single vector or list of 1 or 2 to facet by.
#' @param fill.var An optional variable to fill the code strips by.
#' @param title An optional title for the plot.
#' @param ylab An optional y label.
#' @param xlab An optional x label.
#' @param rev.factor logical.  If \code{TRUE} reverse the current plotting order 
#' so the first element in the plotting variable's levels is plotted on top.
#' @param ncol if an integer value is passed to this 
#' \code{\link[qdap]{gantt_wrap}} uses \code{\link[ggplot2]{facet_wrap}} 
#' rather than \code{\link[ggplot2]{facet_grid}}.
#' @param transform logical.  If \code{TRUE} the repeated facets will be 
#' transformed from stacked to side by side.
#' @param minor.line.freq A numeric value for frequency of minor grid lines.
#' @param major.line.freq A numeric value for frequency of major grid lines.
#' @param sig.dig.line.freq An internal rounding factor for minor and major line 
#' freq.  Generally, default value of 1 suffices for larger range of x scale may 
#' need to be set to -2.
#' @param hms.scale logical.  If \code{TRUE} converts scale to h:m:s format.  
#' Default \code{NULL} attempts to detect if object is a cm_time2long object
#' @param scale Should scales be fixed (\code{"fixed"}, the default), free 
#' (\code{"free"}), or free in one dimension (\code{"free_x"}, \code{"free_y"})
#' @param space If \code{"fixed"}, the default, all panels have the same size. 
#' If \code{"free_y"} their height will be proportional to the length of the y 
#' scale; if \code{"free_x"} their width will be proportional to the length of 
#' the x scale; or if \code{"free"} both height and width will vary. This 
#' setting has no effect unless the appropriate scales also vary.
#' @param size The width of the plot bars.
#' @param rm.horiz.lines logical.  If \code{TRUE} the horizontal lines will be 
#' removed.
#' @param x.ticks  logical.  If \code{TRUE} the x ticks will be displayed.
#' @param y.ticks  logical.  If \code{TRUE} the y ticks will be displayed.
#' @param legend.position The position of legends. (\code{"left"}, 
#' \code{"right"}, \code{"bottom"}, \code{"top"}, or two-element numeric 
#' vector).
#' @param bar.color Optional color to constrain all bars.
#' @param border.color The color to plot border around Gantt bars (default is 
#' \code{NULL}).
#' @param border.size An integer value for the size to plot borders around Gantt 
#' bars. Controls length (width also controlled if not specified).
#' @param border.width Controls border width around Gantt bars.  Use a numeric 
#' value in addition to border size if plot borders appear disproportional.
#' @param constrain logical.  If \code{TRUE} the Gantt bars touch the edge of 
#' the graph. 
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @return Returns a Gantt style visualization. Invisibly returns the ggplot2 
#' list object.
#' @note For non-repeated measures data/plotting use \code{\link[qdap]{gantt}}; 
#' for repeated measures data output use \code{\link[qdap]{gantt_rep}}; and for 
#' a convenient wrapper that takes text and generates plots use 
#' \code{\link[qdap]{gantt_plot}}.
#' @import RColorBrewer 
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha trans_new pretty_breaks
#' @importFrom qdapTools sec2hms
#' @importFrom chron times
#' @importFrom ggplot2 ggplot aes geom_segment geom_vline scale_x_continuous element_rect ggtitle theme element_blank facet_wrap facet_grid guides guide_legend ylab xlab
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
#'     units = "sentences", col.sep = "_")
#' htruncdf(dat)
#' gantt_wrap(dat, "fam.aff_sex", title = "Gantt Plot")
#' dat$codes <- sample(LETTERS[1:3], nrow(dat), TRUE)
#' gantt_wrap(dat, "fam.aff_sex", fill.var = "codes",
#'     legend.position = "bottom")
#' 
#' dat2 <- with(rajSPLIT, gantt_rep(act, dialogue,
#'     list(fam.aff, sex), units = "words", col.sep = "_"))
#' htruncdf(dat2)
#' x <- gantt_wrap(dat2, "fam.aff_sex", facet.vars = "act",
#'     title = "Repeated Measures Gantt Plot")
#' 
#' library(ggplot2); library(scales); library(RColorBrewer)
#' x + scale_color_manual(values=rep("black",
#'     length(levels(dat2$fam.aff_sex))))
#' }
gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, fill.var = NULL, title = NULL, 
    ylab = plot.var, xlab = "duration.default", rev.factor = TRUE,
    transform = FALSE, ncol = NULL, minor.line.freq = NULL, 
    major.line.freq = NULL, sig.dig.line.freq = 1, hms.scale = NULL, 
    scale = NULL, space = NULL, size = 3, rm.horiz.lines = FALSE, x.ticks = TRUE, 
    y.ticks = TRUE, legend.position = NULL, bar.color = NULL, 
    border.color = NULL, border.size = 2, border.width = .1, constrain = TRUE, 
    plot = TRUE) { 
    end <- start <- new <- new4 <- startp <- endp <- NULL
    if (is.null(hms.scale)) {
        if (methods::is(dataframe, "cmtime")) {
            hms.scale <- TRUE
        } else {
            hms.scale <- FALSE
            if (methods::is(dataframe, "cmrange") & xlab == "duration.default"){
                xlab <- "Duration (words)"
            }
        }
    }
    dataframe[, plot.var] <- as.factor(dataframe[, plot.var])
    if (rev.factor) {
        dataframe[, "new"] <- factor(dataframe[, plot.var], 
            levels=rev(levels(dataframe[, plot.var])))
    } else {
        dataframe[, "new"] <- factor(dataframe[, plot.var], 
            levels=levels(dataframe[, plot.var]))
    }
    if(xlab == "duration.default") {
        if (hms.scale) {
                xlab <- "Duration (hours:minutes:seconds)"
        } else {
            if (!is.null(which.unit(dataframe))) {
                xlab <- paste0("Duration (", which.unit(dataframe), ")")
            } else {
                xlab <- "Duration"
            }
        }
    }
    if (!is.null(facet.vars)) {
        dataframe[, facet.vars[1]] <- factor(dataframe[, facet.vars[1]])
        dataframe[, "new2"] <- dataframe[, facet.vars[1]]
        if (length(facet.vars) == 2) {
            dataframe[, facet.vars[2]] <- factor(dataframe[, facet.vars[2]])
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
            dataframe$startp <- as.numeric(sec2hms(dataframe$startp +5))- 
                as.numeric(sec2hms(border.size[1]))
            dataframe$endp <- as.numeric(sec2hms(dataframe$endp))
        }
    } 
    if (hms.scale) {
        if (all(colnames(dataframe) %in% c("Start", "End"))) {
            dataframe$start <- dataframe$Start
            dataframe$end <- dataframe$End
        } else {
            dataframe$start <- sec2hms(dataframe$start)
            dataframe$end <- sec2hms(dataframe$end)
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
    class(theplot)<-c(class(theplot), ifelse(is.null(facet.vars), "gantt_plot", 
        "gantt_plot_m"))
    if (plot) {
        print(theplot)
    }
    invisible(theplot)
}

#' Gantt Plot
#' 
#' \code{gantt_plot} - Animate discourse from \code{\link[qdap]{gantt_wrap}},
#' \code{\link[qdap]{gantt_plot}}, or any other Gantt plotting method.
#' 
#' gantt_plot Method for Animate
#' @param x The gantt_plot object.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param colors An optional character vector of colors to color the Gantt bars.
#' Must be length 1 (repeats the same color) or equal to the levels of the 
#' grouping variable.
#' @param \ldots ignored
#' @export
#' @importFrom qdapTools %l%
#' @method Animate gantt_plot
Animate.gantt_plot <- function(x, wc.time = TRUE, time.constant = 2, 
    colors = NULL, ...){

    x[[c("coordinates", "limits", "x")]] <- c(0, sum(x[["data"]][, "n"]))

    plots <- lapply(0:nrow(x[["data"]]), function(i, myplot=x, dat = x[["data"]]) {
         
        ## Starts the plot with no bars if i = 0
        if (i == 0) {
            thedat <- dat[1, , drop=FALSE]
            thedat[, "end"] <- 0
        } else {
            thedat <- dat[1:i, , drop=FALSE]
        }

        ## Proper coloring
        num <- ifelse(i == 0, 1, i)
        colvar <- thedat[1:num, "new4"]
        lvls <- levels(colvar)
        if (is.null(colors)) {
            lvls_cols <- gg_color_hue(length(lvls))
        } else {
            if (length(colors) == 1) {
                lvls_cols <- rep(colors, length(lvls))
            } else {
                lvls_cols <- colors
            }
        }

        colvars2 <- levels(unique(colvar))[levels(unique(colvar)) %in% unique(colvar)]
        cols <- colvars2 %l% data.frame(lvls, lvls_cols, stringsAsFactors = FALSE)
  
        myplot[["data"]] <- thedat
        myplot + scale_y_discrete(drop=FALSE) + scale_colour_manual(values=cols)
    })

    timings <- round(exp(x[["data"]][, "n"]/(max(x[["data"]][, "n"])/time.constant)))
    if(wc.time) {
        plots <- rep(plots, c(1, timings))
    }
    plots
}

