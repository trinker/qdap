#' Generate Time Spans for Repeated Measures
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param rm.var %% ~~Describe \code{rm.var} here~~
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param units %% ~~Describe \code{units} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author Andrie de Vries and and Tyler Rinker <tyler.rinker@gmail.com>.
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references \url{http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' (dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "sentences",                
#'      plot.colors = 'black', sums = TRUE, col.sep = "_")$gantt.df)     
#' gantt_wrap(dat, fam.aff_sex, title = "Gantt Plot")  
#' dat$codes <- sample(LETTERS[1:3], nrow(dat), TRUE)
#' gantt_wrap(dat, fam.aff_sex, fill.var = "codes", legend.position = "bottom")
#'
#' (dat3 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex), units = "words",                
#'    col.sep = "_")))    
#' gantt_wrap(dat3, fam.aff_sex, facet.vars = "act", title = "Repeated MeasuresGantt Plot")
gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, fill.var = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.default", rev.factor = TRUE,
    transform = FALSE, minor.line.freq = 25, major.line.freq = 100, sig.dig.line.freq = -2,
    scale = NULL, space = NULL, size = 3, rm.horiz.lines = TRUE, x.ticks = FALSE, 
    y.ticks = FALSE, legend.position = NULL, border.color = NULL, border.size = 2) { 
    require(ggplot2)
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
#dataframe[, "new4"] <- factor(dataframe[, plot.var], #remove once I'm sure there's no issues with removing these lines
#levels=rev(levels(dataframe[, plot.var])))
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
            border.size[2] <- size + size*.1
        }
        theplot <- theplot + geom_segment(aes(x=startp, xend=endp, y=new, 
            yend=new), colour = border.color, size=border.size[2], legend.position = "none")  
    }                                                 
    theplot <- theplot + geom_segment(aes(x=start, xend=end, y=new, yend=new), 
        size=size) +  
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
                theplot <- theplot + facet_grid(.~new2, scale = scale, space = space)           
            } else {
                theplot <- theplot + facet_grid(new2~., scale = scale, space = space)
            }
        } else {
            theplot <- theplot + facet_grid(new2~new3, scale = scale, space = space)
        }
    }         
    print(theplot)
    invisible(theplot)
}