#' Visualize Word Length by Turn of Talk
#' 
#' Uses a bar graph to visualize patterns in sentence length and grouping 
#' variables by turn of talk.
#' 
#' @param dataframe A dataframe that contains the text variable and optionally 
#' the grouping.var and tot variables.
#' @param text.var The text variable (character string).
#' @param grouping.var The grouping variables to color by.  Default \code{NULL} 
#' colors everything in "black".  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables. 
#' @param facet.vars An optional single vector or list of 1 or 2 to facet by.
#' @param tot The turn of talk variable (character string). May be \code{TRUE} 
#' (assumes "tot" is the variable name), \code{FALSE} (use row numbers), or a 
#' character string of the turn of talk column.
#' @param ncol number of columns. 
#' \code{\link[qdap]{gantt_wrap}} uses \code{\link[ggplot2]{facet_wrap}} 
#' rather than \code{\link[ggplot2]{facet_grid}}.
#' @param transform logical.  If \code{TRUE} the repeated facets will be 
#' transformed from stacked to side by side.
#' @param ylab Optional y label.
#' @param xlab Optional x label.
#' @param bar.space The amount space between bars (ranging between 1 and 0).
#' @param scale Should scales be fixed (\code{"fixed"}, the default), free 
#' (\code{"free"}), or free in one dimension (\code{"free_x"}, \code{"free_y"})
#' @param space If \code{"fixed"}, the default, all panels have the same size. 
#' If \code{"free_y"} their height will be proportional to the length of the y 
#' scale; if \code{"free_x"} their width will be proportional to the length of 
#' the x scale; or if \code{"free"} both height and width will vary. This 
#' setting has no effect unless the appropriate scales also vary.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @return Invisibly returns the ggplot2 object.
#' @import RColorBrewer
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom ggplot2 ggplot aes scale_y_continuous geom_bar theme element_blank facet_wrap facet_grid xlab ylab
#' @export
#' @examples
#' \dontrun{
#' dataframe <- sentSplit(DATA, "state")
#' tot_plot(dataframe, "state")
#' tot_plot(DATA, "state", tot=FALSE)
#' tot_plot(dataframe, "state", bar.space=.03)
#' tot_plot(dataframe, "state", "sex")
#' tot_plot(dataframe, "state", "person", tot = "sex")
#' tot_plot(mraja1, "dialogue", "fam.aff", tot=FALSE)
#' tot_plot(mraja1, "dialogue", "died", tot=FALSE)
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE) + 
#'     scale_fill_hue(l=40) 
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)+ 
#'     scale_fill_brewer(palette="Spectral")
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)+ 
#'     scale_fill_brewer(palette="Set1")
#'
#' ## repeated measures
#' rajSPLIT2 <- do.call(rbind, lapply(split(rajSPLIT, rajSPLIT$act), head, 25))
#' tot_plot(rajSPLIT2, "dialogue", "fam.aff", facet.var = "act")
#' 
#' ## add mean and +/- 2 sd
#' tot_plot(mraja1, "dialogue", grouping.var = c("sex", "fam.aff"), tot=FALSE)+
#'     scale_fill_brewer(palette="Set1") +
#'     geom_hline(aes(yintercept=mean(word.count))) +
#'     geom_hline(aes(yintercept=mean(word.count) + (2 *sd(word.count)))) +
#'     geom_hline(aes(yintercept=mean(word.count) + (3 *sd(word.count)))) +
#'     geom_text(parse=TRUE, hjust=0, vjust=0, family="serif", size = 4, aes(x = 2, 
#'         y = mean(word.count) + 2, label = "bar(x)")) +
#'     geom_text(hjust=0, vjust=0, family="serif", size = 4, aes(x = 1, 
#'         y = mean(word.count) + (2 *sd(word.count)) + 2, label = "+2 sd")) +
#'     geom_text(hjust=0, vjust=0, family="serif", size = 4, aes(x = 1, 
#'         y = mean(word.count) + (3 *sd(word.count)) + 2, label = "+3 sd")) 
#' }
tot_plot <- function(dataframe, text.var, grouping.var = NULL, facet.vars = NULL, 
    tot = TRUE, transform = FALSE, ncol = NULL, ylab=NULL, xlab=NULL, bar.space=0, 
    scale = NULL, space = NULL, plot = TRUE) {
    word.count <- group <- caps <- NULL

    DF <- dataframe

    if (is.logical(tot)) {

        if (isTRUE(tot)) {

            ## If TRUE use tot column name
            if (!"tot" %in% colnames(dataframe)) {
                stop("supply valid tot argument")
            }
            tot <- TOT(dataframe[["tot"]])

        } else {

            if (!is.null(facet.vars)) {
                ## Create ID variable
                DF[, "qdapIDqdap"] <- seq_len(nrow(DF))
            
                ## Split, order and make tot
                rmout <- lapply(split(DF, DF[[facet.vars]]), function(x) {
                    x <- x[order(x[, "qdapIDqdap"]), ]
                    x[, "tot"] <- seq_len(nrow(x))
                    x
                })
            
                ## glue together and order by ID
                rmout <- do.call(rbind, rmout)
                rmout <- rmout[order(rmout[, "qdapIDqdap"]), ]
                tot <- rmout[, "tot"]
                DF[, "qdapIDqdap"] <- NULL    
            
            } else {
                        
                tot <- seq_len(nrow(DF))
            }

        }

    } else {

        if (is.character(tot)) {
            lentot <- length(tot)

            ## warning if tot is character & not = to 1 or nrow of dataframe
            if (lentot != 1 && lentot != nrow(DF)) {
                stop("tot not = to nrow of dataframe")
            }

            ## single length characteruse that column from dataframe
            ## otherwise treat character vector as the tot column
            if (lentot == 1) {
                tot <- dataframe[, tot]
            } 


            a <- rle(as.character(tot))
            tot <- rep(seq_along(a$lengths), a$lengths)

        }

    }

    dataframe <- data.frame(tot = tot, text.var = dataframe[, text.var])

    ## add grouping variables
    if (!is.null(grouping.var)) {
        G <- paste(grouping.var, collapse="&")
        if (ncol(DF[, grouping.var, drop=FALSE]) > 1) {
            dataframe[, "group"] <- paste2(DF[, grouping.var])
        } else {
            dataframe[, "group"] <- DF[, grouping.var]
        }
    }

    ## add facet variables
    if (!is.null(facet.vars)) {
        G2 <- paste(facet.vars, collapse="&")
        if (ncol(DF[, facet.vars, drop=FALSE]) > 1) {

            dataframe[, "new2"] <- DF[, facet.vars[1]]
            dataframe[, "new3"] <- DF[, facet.vars[2]]

        } else {
            dataframe[, "new2"] <- DF[, facet.vars[1]]
        }
    }

    dataframe[, "word.count"] <- wc(dataframe[, "text.var"])

    if (is.null(xlab)) {
        Xlab <- "Turn of Talk"
    }
    if (is.null(ylab)) {
        Ylab <- "Word Count"
    }

    dataframe <- stats::na.omit(dataframe)
    dataframe <- droplevels(dataframe)
    dataframe[, "bar.space"] <- rep(bar.space, nrow(dataframe))

    dataframe[, "tot"] <- factor(dataframe[, "tot"], 
       levels= sort(unique(dataframe[, "tot"])))

    ## PLOTTING
     
    ## base plot
    theplot <- ggplot(dataframe, aes(x = tot)) 

    ## add grouping variable
    if (!is.null(grouping.var)) {
        theplot <- theplot + geom_bar(aes(weight = word.count, fill = group), 
            width= 1-bar.space, data=dataframe) +
            labs(fill = Caps(gsub("&", " & ", G, fixed=TRUE), all=TRUE)) 
    } else {
        theplot <- theplot + 
            geom_bar(aes(weight = word.count), width= 1-bar.space, data=dataframe)
    }

    theplot <- theplot + ylab(Ylab) + xlab(Xlab) + 
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
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
    if (plot) {
        print(theplot)
    }
    invisible(theplot) 
}
