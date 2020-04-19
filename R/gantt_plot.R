#' Gantt Plot
#'
#' A convenience function that wraps \code{\link[qdap]{gantt}}, 
#' \code{\link[qdap]{gantt_rep}} and \code{\link[qdap]{gantt_wrap}} into a single 
#' plotting function.
#'
#' @param text.var The text variable.    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param rm.var An optional single vector or list of 1 or 2 of repeated 
#' measures to facet by    
#' @param fill.var An optional variable to fill the code strips by. 
#' @param xlab The name of the x-axis label.
#' @param units The unit of measurement.      
#' @param col.sep The column separator.
#' @param \ldots Other arguments passed to \code{\link[qdap]{gantt_wrap}}.
#' @return Returns a Gantt style visualization.  Invisibly returns the ggplot2 
#' list object.
#' @note For non-repeated measures data/plotting use \code{\link[qdap]{gantt}}; 
#' for repeated measures data output use \code{\link[qdap]{gantt_rep}}; and for 
#' a flexible gantt plot that words with code matrix functions (cm) use 
#' \code{\link[qdap]{gantt_wrap}}.
#' @seealso \code{\link[qdap]{gantt}}, 
#' \code{\link[qdap]{gantt_rep}},
#' \code{\link[qdap]{gantt_wrap}}
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @importFrom grid unit
#' @examples
#' \dontrun{
#' with(rajSPLIT, gantt_plot(text.var = dialogue,
#'     grouping.var = person, size=4))
#' 
#' with(rajSPLIT, gantt_plot(text.var = dialogue,
#'     grouping.var = list(fam.aff, sex), rm.var  = act,
#'     title = "Romeo and Juliet's dialogue"))
#' 
#' with(rajSPLIT, gantt_plot(dialogue, list(fam.aff, sex), act,
#'     transform=T))
#' 
#' rajSPLIT2 <- rajSPLIT
#' 
#' rajSPLIT2$newb <- as.factor(sample(LETTERS[1:2], nrow(rajSPLIT2),
#'     replace=TRUE))
#' 
#' z <- with(rajSPLIT2, gantt_plot(dialogue, list(fam.aff, sex),
#'     list(act, newb), size = 4))
#' 
#' library(ggplot2); library(scales); library(RColorBrewer); library(grid)
#' z + theme(panel.spacing = unit(1, "lines")) + scale_colour_grey()
#' z + scale_colour_brewer(palette="Dark2")
#' 
#' ## Fill Variable Example
#' dat <- rajSPLIT[rajSPLIT$act == 1, ]
#' dat$end_mark <- factor(end_mark(dat$dialogue))
#' 
#' with(dat, gantt_plot(text.var = dialogue, grouping.var = list(person, sex),
#'     fill.var=end_mark))
#' 
#' ## Repeated Measures with Fill Example
#' rajSPLIT$end_mark <- end_mark(rajSPLIT$dialogue)
#' 
#' with(rajSPLIT, gantt_plot(text.var = dialogue,
#'     grouping.var = list(fam.aff), rm.var  = list(act),
#'     fill.var=end_mark, title = "Romeo and Juliet's dialogue"))
#' 
#' ## Repeated Measures Sentence Type Example
#' with(rajSPLIT, gantt_plot(text.var = dialogue,
#'     grouping.var = list(fam.aff, sex), rm.var  = list(end_mark, act),
#'     title = "Romeo and Juliet's dialogue"))
#' 
#' ## Reset rajSPLIT
#' rajSPLIT <- qdap::rajSPLIT
#' 
#' ## Animate It
#' ##=================
#' ani_gantt <- with(mraja1, gantt_plot(dialogue, person))
#' 
#' library(animation)
#' loc <- folder(animation_gantt)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN <- function() {
#'     out <- Animate(ani_gantt)
#'     lapply(out, function(x) {
#'         print(x)
#'         animation::ani.pause()
#'     })
#' 
#' }
#' 
#' type <- if(.Platform$OS.type == "windows") shell else system
#' saveVideo(FUN(), video.name = "animation.avi", interval = 0.1, outdir = loc)
#' 
#' saveLatex(FUN(), autoplay = TRUE, loop = FALSE, latex.filename = "tester.tex", 
#'     caption = "animated dialogue", outdir = loc, ani.type = "pdf", 
#'     ani.dev = "pdf", ani.width = 5, ani.height = 5.5, interval = 0.1)
#' 
#' 
#' saveHTML(FUN(), autoplay = FALSE, loop = TRUE, verbose = FALSE, 
#'     ani.width=600, ani.height=280,
#'     outdir = file.path(loc, "new"), single.opts = 
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' }
gantt_plot <- 
function(text.var, grouping.var = NULL, rm.var = NULL, fill.var = NULL, 
    xlab = "duration (in words)", units = "words", col.sep = "__", ...) {

    if(is.null(grouping.var)) {
        PNAMES <- ANAMES <- NAME <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            PNAMES <- ANAMES <- m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
            NAME <- paste(m, collapse=col.sep)
        } else {
            G <- as.character(substitute(grouping.var))
            PNAMES <- ANAMES <- NAME <- G[length(G)]
        }
    }


    if(is.null(grouping.var)){
        grouping.var <- rep("all", length(text.var))
    } 

    if (!is.null(fill.var)) {
        if (is.list(fill.var)) {
            m <- unlist(as.character(substitute(fill.var))[-1])
            FNAMES <- m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
            ANAMES <- c(ANAMES, m)
            fillNAME <- paste(m, collapse=col.sep)
        } else {
            G <- as.character(substitute(fill.var))
            FNAMES <- fillNAME <- G[length(G)]
            ANAMES <- c(ANAMES, fillNAME)
        } 
    } else {
        fillNAME <- NULL
    }
    
    if(!is.list(grouping.var)) {
        grouping.var <- list(grouping.var)
    }
    glen <- length(grouping.var)
    flen <- 0

    ## If not null fill var combine with grouping var
    if (!is.null(fillNAME)) {
        if(!is.list(fill.var)) {
            fill.var <- list(fill.var)
        }
        
        ## Lengths on grouping.var and fill.var
        ## Used later for sending columns to gantt_wrap
        flen <- length(fill.var)
        
        vars <- unlist(list(grouping.var, fill.var), recursive = FALSE)
    } else {
        vars <- grouping.var
        FNAMES <- NULL
    }

    ## getting the start end times 
    if (is.null(rm.var)) {
        ## getting the start end times for non-repeated measures
        x <- gantt(text.var = text.var, grouping.var = vars, 
            units =units, col.sep = "%%%%")
        x <- colsplit2df(x, new.names = ANAMES, sep="%%%%")
        repNAMES <- rmNAME <- NULL
        rlen <- 0
    } else {
        if (is.list(rm.var)) {
            ## getting the start end times for repeated measures
            m <- unlist(as.character(substitute(rm.var))[-1])
            repNAMES <- m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)])
            rmNAME <- paste(m, collapse=col.sep)
        } else {
            G <- as.character(substitute(rm.var))
            repNAMES <- rmNAME <- G[length(G)]
        }
        x <- gantt_rep(rm.var = rm.var, text.var = text.var, 
            grouping.var = vars, units =units, col.sep = "%%%%")

        if (!is.null(rmNAME)) {
            colnames(x)[1:2] <- c(rmNAME, NAME)
        } else {
            colnames(x)[1] <- NAME
        }

        x <- colsplit2df(x, splitcols = 2, new.names = ANAMES, sep = "%%%%")
        rlen <- length(repNAMES)
        if (rlen > 1) {
            x <- colsplit2df(x, splitcols = 1, new.names = repNAMES, sep = "%%%%")
        } else {
            colnames(x)[length(repNAMES)] <- repNAMES
        }
    }
      
    if (any(c(repNAMES, PNAMES, FNAMES) %in% c("end", "start"))) {
        stop("The names `start` and `end` may not be used")
    }
  
    ##lens <- c(rlen, glen, flen)
    if (rlen > 1) {
        x[, rmNAME] <- paste2(x[, repNAMES], sep = col.sep)
    } else {
        if (rlen == 1) {
            x[, rmNAME] <- x[, repNAMES]
        } 
    }
    
    if (glen > 1) {
        x[, NAME] <- paste2(x[, PNAMES], sep = col.sep)
    } else {
        if (rlen == 1) {
            x[, NAME] <- x[, PNAMES]
        } 
    }
    
    if (flen > 1) {
        x[, fillNAME] <- paste2(x[, FNAMES], sep = col.sep)
    } else {
        if (rlen == 1) {
            x[, fillNAME] <- x[, FNAMES]
        } 
    }

    y <- gantt_wrap(dataframe = x, plot.var = NAME, facet.vars = repNAMES, 
         fill.var = fillNAME, xlab = xlab, ...)

    invisible(y)
}
