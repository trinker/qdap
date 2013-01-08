#' Gantt Plot
#'
#' A convenience function that wraps \code{gantt}, \code{gantt_rm} and 
#' \code{gantt_wrap} into a single 
#' plotting function.
#'
#' @param text.var The text variable.    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param rm.var An optional single vector or list of 1 or 2 of repeated 
#' measures to facet by    
#' @param fill.var An optional variable to fill the code stips by. 
#' @param units The unit of measurement.      
#' @param col.sep The column separator.
#' @param \ldots Other arguments passed to gantt_wrap.
#' @return Returns a Gantt style visualization.
#' @note For non repeated measures data/plotting use \code{gantt}; for repeated 
#' measures data output use \code{gantt_rep}; and for a flexible gantt plot that 
#' words with code matrix functions (cm) use \code{gantt_wrap}.
#' @seealso \code{\link[qdap]{gantt}} 
#' \code{\link[qdap]{gantt_rep}},
#' \code{\link[qdap]{gantt_wrap}},
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @examples
#' \dontrun{
#' with(rajSPLIT, gantt_plot(text.var = dialogue, grouping.var = person, size=4))
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
#' }
gantt_plot <- 
function(text.var, grouping.var, rm.var = NULL, fill.var = NULL, 
    xlab = "duration (in words)", units = "words", col.sep = "_", ...) {
    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        NAME <- paste(m, collapse=col.sep)
    } else {
        G <- as.character(substitute(grouping.var))
        NAME <- G[length(G)]
    }
    if (is.null(rm.var)) {
        x <- gantt(text.var = text.var, grouping.var = grouping.var, 
            plot = FALSE, units =units, col.sep = col.sep)
        colnames(x)[ncol(x)-3] <- NAME
        initial <- unlist(strsplit(NAME, col.sep))
        colnames(x)[1:length(initial)] <- initial
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
    if (!is.null(fill.var)){
        if (is.list(fill.var)) {
            m <- unlist(as.character(substitute(fill.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
            fillNAME <- paste(m, collapse=col.sep)
        } else {
            G <- as.character(substitute(fill.var))
            fillNAME <- G[length(G)]
        }


        if (!is.list(fill.var)){
            fill.var <- list(fill.var)
        }
        if (length(fill.var) > 1) {
            fill.var <- paste2(fill.var)
        }
        fv <- data.frame(fill.var=fill.var, text.var=text.var, 
            grouping.var=grouping.var)
        if (!is.null(rm.var)) {
            fv$rm.var <- rm.var
        }
        colnames(fv)[1] <- "fill.var"
        fv2 <- fv[!is.na(text.var), -2]
        if (!is.null(rm.var)) {
            fv2 <- data.frame(paste2(fv2[, 3:2]), fv2[, 1, drop=FALSE])
        } else {
            fv2 <- fv2[, 2:1]
        }
        names(fv2)[1] <- "key"
        fv2 <- unique(fv2)
        nx <- ncol(x)
        kv <- x[, -c((nx-2):nx)]
        if (ncol(kv) > 1) {
            x[, "key"] <- paste2(kv)
        } else {
            x[, "key"] <- kv
        }
        x$fill_var <- lookup(x$key, fv2)
        x[, "key"] <- NULL
        colnames(x)[ncol(x)] <- fillNAME

    }
    y <- gantt_wrap(dataframe = x, plot.var = NAME, facet.vars = rmNAME, 
         fill.var = fillNAME, xlab = xlab, ...)
    invisible(y)
}
