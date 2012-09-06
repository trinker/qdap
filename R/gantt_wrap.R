gantt_wrap <-
function(dataframe, plot.var, facet.vars = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.defalut", rev.factor = TRUE,
    transform = FALSE, minor.line.freq = 5, major.line.freq = 25) { 
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
                theplot <- theplot + facet_grid(.~new2)           
            } else {
                theplot <- theplot + facet_grid(new2~.)
            }
        } else {
            theplot <- theplot + facet_grid(new2~new3)
        }
    }         
    print(theplot)
}
