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
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' (dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "sentences",                
#'      plot.colors = 'black', sums = TRUE, col.sep = "_")$gantt.df)     
#' gantt_wrap(dat, fam.aff_sex, title = "Gantt Plot")  
#' 
#' (dat3 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex), units = "words",                
#'    col.sep = "_")))    
#' gantt_wrap(dat3, fam.aff_sex, facet.vars = "act", title = "Repeated MeasuresGantt Plot",
#'    minor.line.freq = 25, major.line.freq = 100)
gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.defalut", rev.factor = TRUE,
    transform = FALSE, minor.line.freq = 5, major.line.freq = 25, scale = "free") { 
    require(ggplot2)
    plot.var <- as.character(substitute(plot.var))
    if (rev.factor) {
        dataframe[, "new"] <- factor(dataframe[, plot.var], levels=rev(levels(dataframe[, plot.var])))
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
    theplot <- ggplot(dataframe, aes(colour=new)) +                        
        geom_vline(xintercept = seq(0, round(max(dataframe$end), -2), 
           minor.line.freq), colour="gray92", size = .025) +                                                        
        geom_vline(xintercept = seq(0, round(max(dataframe$end), -2),            
           major.line.freq), colour="gray50", size = .05) +                                                            
        geom_segment(aes(x=start, xend=end, y=new, yend=new), size=2) +  
        ylab(ylab) +    
        xlab(xlab) +                   
        theme_bw() +                                                                  
        scale_x_continuous(expand = c(0,0))+                                     
        theme(panel.background = element_rect(fill=NA, color="black"),       
           legend.position = "none", legend.position = "none",
           panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank()) +
        ggtitle(title)  
    if (!is.null(facet.vars)) { 
        if (length(facet.vars) == 1) {
            if (transform) {
                theplot <- theplot + facet_grid(.~new2, scale = scale)           
            } else {
                theplot <- theplot + facet_grid(new2~., scale = scale)
            }
        } else {
            theplot <- theplot + facet_grid(new2~new3, scale = scale)
        }
    }         
    print(theplot)
}
