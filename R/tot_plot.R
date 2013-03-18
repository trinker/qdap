#' Visualize Word Length by Turn of Talk
#' 
#' Uses a bar graph to visualize patterns in sentence length and grouping 
#' variables by turn of talk.
#' 
#' @param dataframe A dataframe that contains the text variable and optionally 
#' the grouping.var and tot variables.
#' @param text.var The text variable (character string).
#' @param grouping.var The grouping variables (character string).  
#' @param tot The turn of talk variable (character string). May be TRUE (assumes 
#' "tot" is the variable name), FALSE (use row numbers), or a character string 
#' of the turn of talk column.
#' @param ylab Optional y label.
#' @param xlab Optional x label.
#' @param space The amount space between bars (ranging between 1 and 0).
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
#' tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)
#' }
tot_plot <- function(dataframe, text.var, grouping.var = NULL, tot = TRUE, 
    ylab=NULL, xlab=NULL, space=0) {
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
    colnames(dataframe)[2] <- "text.var"
    dataframe[, "word.count"] <- wc(dataframe[, "text.var"])
    if (is.null(xlab)) {
        Xlab <- "Turn of Talk"
    }
    if (is.null(ylab)) {
        Ylab <- "Word Count"
    }
    dataframe[, "tot"] <- factor(dataframe[, "tot"], 
        levels=sort(as.numeric(as.character(dataframe[, "tot"]))))
    dataframe["space"] <- rep(space, nrow(dataframe))
    theplot <- ggplot(dataframe, aes(tot, word.count, width=1-space)) 
    if (ncol(dataframe) == 5) {
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
    print(theplot)
    invisible(theplot) 
}
