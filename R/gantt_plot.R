#' Gantt Plot
#'
#' A convenience that wraps gantt, gantt_rm and gantt_wrap into a single plotting function.
#'
#' @param text.var The text variable    
#' @param grouping.var The grouping variables. Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param rm.var      
#' @param units       
#' @param col.sep     
#' @param \\ldots   
#' @return   
#' @seealso
#' @references\url{http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' with(rajSPLIT, gantt_plot(text.var = dialogue, grouping.var = person,
#'     minor.line.freq = NULL, major.line.freq = NULL, size=4))
#' with(rajSPLIT, gantt_plot(text.var = dialogue, grouping.var = 
#'     list(fam.aff, sex), rm.var  = act, 
#'     title = "Romeo and Juliet's dialogue"))
#' with(rajSPLIT, gantt_plot(dialogue, list(fam.aff, sex), act, transform=T))
#' rajSPLIT2 <- rajSPLIT
#' rajSPLIT2$newb <- as.factor(sample(LETTERS[1:2], nrow(rajSPLIT2), 
#'     replace=TRUE))
#' z <- with(rajSPLIT2, gantt_plot(dialogue, list(fam.aff, sex), 
#'     list(act, newb), size = 4))
#' z + theme(panel.margin = unit(1, "lines")) + scale_colour_grey()
#' z + scale_colour_brewer(palette="Dark2")
gantt_plot <-
function(text.var, grouping.var, rm.var = NULL, 
    units = "words", col.sep = "_", ...) {
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        paste(m, collapse=col.sep)
    } else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    if (is.null(rm.var)) {
        x <- gantt(text.var = text.var, grouping.var = grouping.var, 
            plot = FALSE, units =units, col.sep = col.sep)
        colnames(x)[1] <- NAME
    } else {
        rmNAME <- if (is.list(rm.var)) {
            m <- unlist(as.character(substitute(rm.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
            paste(m, collapse=col.sep)
        } else {
            G <- as.character(substitute(rm.var))
            G[length(G)]
        }
        x <- gantt_rep(rm.var = rm.var, text.var = text.var, 
            grouping.var = grouping.var, units =units, col.sep = col.sep)
        colnames(x)[1:2] <- c(rmNAME, NAME)
    }
    if (is.null(rm.var)) {
        rmNAME <- NULL
    }
    if(length(rm.var) == 2) {
        x <- colsplit2df(x, sep = ".")
        rmNAME <- unlist(strsplit(rmNAME, col.sep, fixed=TRUE))
        colnames(x)[1:2] <- rmNAME
    }
    y <- gantt_wrap(dataframe = x, plot.var = NAME, facet.vars = rmNAME, ...)
    invisible(y)
}
