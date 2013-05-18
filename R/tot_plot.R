#' Visualize Word Length by Turn of Talk
#' 
#' Uses a bar graph to visualize patterns in sentence length and grouping 
#' variables by turn of talk.
#' 
#' @param dataframe A dataframe that contains the text variable and optionally 
#' the grouping.var and tot variables.
#' @param text.var The text variable (character string).
#' @param grouping.var The grouping variables (character string).  
#' @param facet.vars An optional single vector or list of 1 or 2 to facet by.
#' @param tot The turn of talk variable (character string). May be TRUE (assumes 
#' "tot" is the variable name), FALSE (use row numbers), or a character string 
#' of the turn of talk column.
#' @param ncol if an integer value is passed to this 
#' \code{\link[qdap]{gantt_wrap}} uses \code{\link[ggplot2]{facet_wrap}} 
#' rather than \code{\link[ggplot2]{facet_grid}}.
#' @param transform logical.  If TRUE the repeated facets will be transformed 
#' from stacked to side by side.
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
#' @return Invisibly returns the ggplot2 object.
#' @keywords sentence, split, turn-of-talk
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' dataframe <- sentSplit(DATA, "state")
#' tot_plot(dataframe, "state")
#' tot_plot(DATA, "state", tot=FALSE)
#' tot_plot(dataframe, "state", space=.03)
#' tot_plot(dataframe, "state", "sex")
#' tot_plot(mraja1, "dialogue", "fam.aff", tot=FALSE)
#' tot_plot(mraja1, "dialogue", "died", tot=FALSE)
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE) + 
#'     scale_fill_hue(l=40) 
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)+ 
#'     scale_fill_brewer(palette="Spectral")
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)+ 
#'     scale_fill_brewer(palette="Set1")
#' }
tot_plot <- function(dataframe, text.var, grouping.var = NULL, facet.vars = NULL, 
    tot = TRUE, transform = FALSE, ncol = NULL, ylab=NULL, xlab=NULL, bar.space=0, 
    scale = NULL, space = NULL) {
    group <- caps <- NULL
    DF <- dataframe
    if (isTRUE(tot)) {
        if(!any(colnames(dataframe) %in% "tot")) {
            warning("Turn of talk (\"tot\") column not found; using rows instead")
            tot2 <- dataframe[, "tot"] <- 1:nrow(dataframe)
            dataframe <- dataframe[, c("tot", text.var)]
        } else {
            tot2 <- tot <- TOT(dataframe[, "tot"])
            dataframe <- sentCombine(dataframe[, text.var], tot)
            tot <- TRUE
        }
    }
    if (!tot) {
        tot2 <- dataframe[, "tot"] <- 1:nrow(dataframe)
        dataframe <- dataframe[, c("tot", text.var)]
    } 
    if (is.character(tot)) {
        if(!any(colnames(dataframe) %in% tot)) {
            warning("Turn of talk (", tot, ") column not found; using rows instead")
            tot2 <- dataframe[, "tot"] <- 1:nrow(dataframe)
            dataframe <- dataframe[, c("tot", text.var)]
        } else {
            tot2 <- tot
            dataframe <- sentCombine(dataframe[, text.var], tot)
        }
    }
    if (!is.null(grouping.var)) {
        G <- paste(grouping.var, collapse="&")
        if (ncol(DF[, grouping.var, drop=FALSE]) > 1) {
            dataframe[, "group"] <- sapply(split(paste2(DF[, grouping.var]), tot2), unique)
        } else {
            dataframe[, "group"] <- sapply(split(DF[, grouping.var], tot2), unique)
        }
        colnames(dataframe)[3] <- G
    }
    if (!is.null(facet.vars)) {
        G2 <- paste(facet.vars, collapse="&")
        if (ncol(DF[, facet.vars, drop=FALSE]) > 1) {
            dataframe[, "new2"] <- sapply(split(paste2(DF[, facet.vars[1]]), tot2), unique)
        } else {
            dataframe[, "new2"] <- sapply(split(DF[, facet.vars[1]], tot2), unique)
        }
        if (length(facet.vars) == 2) {
            if (ncol(DF[, facet.vars, drop=FALSE]) > 1) {
                dataframe[, "new3"] <- sapply(split(paste2(DF[, facet.vars[2]]), tot2), unique)
            } else {
                dataframe[, "new3"] <- sapply(split(DF[, facet.vars[2]], tot2), unique)
            }
        } 
    }
    colnames(dataframe)[2] <- "text.var"
    dataframe[, "word.count"] <- wc(dataframe[, "text.var"])
    if (is.null(xlab)) {
        Xlab <- "Turn of Talk"
    }
    if (is.null(ylab)) {
        Ylab <- "Word Count"
    }
    dataframe <- na.omit(dataframe)
    dataframe[, "tot"] <- factor(dataframe[, "tot"], 
        levels=sort(as.numeric(as.character(dataframe[, "tot"]))))
    dataframe <- dataframe[order(dataframe[, "tot"]), ]
    dataframe <- droplevels(dataframe)
    if (!is.null(facet.vars)) {       
        if (length(facet.vars == 1)) {
            sdat <- split(dataframe, dataframe[, "new2"])
        } else {
            sdat <- split(dataframe, paste2(dataframe[, c("new2", "new3")]))
        }
        sdat <- lapply(sdat, function(x) {
             x[, "tot"] <- factor(1:nrow(x), levels=1:nrow(x))
             x
        })
        dataframe <- do.call(rbind.data.frame, sdat)
    }
    dataframe["bar.space"] <- rep(bar.space, nrow(dataframe))
    theplot <- ggplot(dataframe, aes(tot, word.count, width=1-bar.space)) 
    if (!is.null(grouping.var)) {
        GR <- colnames(dataframe)[3]
        colnames(dataframe)[3] <- "group"
        theplot <- theplot + geom_bar(stat="identity", aes(fill=group), data=dataframe) +
            labs(fill = caps(gsub("&", " & ", GR, fixed=TRUE), all=TRUE)) 
    } else {
        theplot <- theplot + geom_bar(stat="identity")
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
    return(theplot) 
}
