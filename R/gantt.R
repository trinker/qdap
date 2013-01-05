#' Generate Unit Spans 
#' 
#' Generates start and end times of supplied text selections (i.e. text 
#' selections are determined by any number of grouping variables).
#' 
#' @param text.var The text variable    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param plot logical.  If TRUE plots the start-end times as a gantt plot.
#' @param units The unit of measurement to analyze.  One of the strings 
#' \code{"character"}, \code{"syllable"}, \code{"word"}, or \code{"sentence"}.
#' @param sums logical.  If TRUE reports and optionally plots the total units 
#' used by grouping variable(s).
#' @param plot.colors The colors of the Gannt plot bars.  Either a single color 
#' or a length equal to the number of grouping variable(s).
#' @param box.color A single color of the box around the Gantt plot bars.
#' @return Returns a data frame of start and end times by grouping variable(s) 
#' or optionally returns a list of two: (1) A data frame of the total units 
#' used by grouping variable(s) and (2) a data frame of of start and end times 
#' by grouping variable(s).  Optionally plots a gantt plot of the returned data.
#' @note For repeated measures data output use \code{gantt_rep}; for a convientent 
#' wrapper that takes text and generates plots use \code{gantt_plot}; and for a 
#' flexible gantt plot that words with code matrix functions (cm) use 
#' \code{gantt_wrap}.
#' @author DigEmAll (\url{stackoverflow.com}) and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso \code{\link[qdap]{gantt_rep}},
#' \code{\link[qdap]{gantt_wrap}},
#' \code{\link[qdap]{gantt_plot}} 
#' @references Wallace Clark and Henry Gantt (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @examples
#' \dontrun{
#' gantt(DATA$state, DATA$person)                                                        
#' gantt(DATA$state, DATA$person, sums = TRUE)                                           
#' gantt(DATA$state, list(DATA$sex, DATA$adult))                                                           
#' gantt(mraja1$dialogue, mraja1$person) #hard to see without box color   
#' gantt(mraja1$dialogue, mraja1$sex)                       
#' gantt(mraja1$dialogue, mraja1$person, box.col = "black")                                      
#' gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), plot.colors = NULL)                         
#' gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), plot.colors = "black")                      
#' gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), plot = FALSE)                                                                                                                       
#' gantt(mraja1$dialogue, mraja1$person, units = "characters", box.color = "black")              
#' gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "characters")                       
#' with(mraja1, gantt(dialogue, list(fam.aff, sex, died), 
#'    units = "characters", sums = TRUE))       
#' gantt(mraja1$dialogue, mraja1$person, units = "syllables", box.color = "black", sums = TRUE)  
#' gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "syllables")                        
#' 
#' (dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex), units = "sentences",                
#'      plot.colors = 'black', sums = TRUE, col.sep = "_")$gantt.df)     
#' gantt_wrap(dat, fam.aff_sex, title = "Gantt Plot")  
#' }
gantt <-
function(text.var, grouping.var, plot = TRUE, units = "words", 
    sums = FALSE, plot.colors = NULL, box.color = NULL, col.sep = "_"){
    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        NAME <- paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        NAME <- G[length(G)]
    }
    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping.var <- apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))) {
                    NA 
                } else {
                    paste(x, collapse = ".") 
                }
            }
        )
    } else {
        grouping.var <- grouping.var
    }
    g <- factor(grouping.var)
    grouping.var <- factor(grouping.var)
    if (is.list(grouping.var)) {
        LEVS <- lapply(grouping.var, levels)
        LEVS2 <- paste2(do.call(expand.grid, LEVS))
    } else {
        LEVS2 <- levels(grouping.var)
    }
    DF <- data.frame(text = as.character(text.var), 
        group = grouping.var, stringsAsFactors = FALSE)
    names(DF) <- c("text", "group")
    DF$group <- factor(DF$group, levels=LEVS2)
    k <- rle(as.numeric(DF$group))
    id <- rep(seq_along(k$len), k$len)
    out <- tapply(DF$text, id, paste, collapse = " ")
    ans <- data.frame(text = out, group = levels(DF$group)[k$val])
    switch(units,
        words = ner <- function(x)   length(unblanker(words(strip(x)))),
        characters = ner <- function(x) nchar(gsub(" ", "", x)),
        syllables = ner <- function(x) syllable.sum(x), 
        sentences = {ner <- function(x) {
            p <- sum(gregexpr("[.?!*_]", x)[[1]] > 0)
            if (p==0)1 else p
            }
        }
    )
    n <- sum(ner(text.var))
    ans$n <- sapply(ans$text, ner)
    ans$end <- cumsum(ans$n)
    ans$start <- c(0, ans$end[-length(ans$end)])
    ans <- ans[, c(2:3, 5, 4)]
    names(ans)[1] <- NAME                            
    z <- tapply(ans[, "n"], ans[, 1], FUN=sum)
    z <- data.frame(names(z), total = z)
    names(z)[1] <- NAME
    z <- z[order(z[, 1]), ]
    rownames(z) <- NULL
    if (plot) {
        if (is.null(box.color)) box.color <- "white" 
        y2 <- NULL
        if(sums) y2 <- z[, 2] 
        if (is.null(plot.colors)) {
            plot.colors <- rainbow(10 + length(levels(ans[, 1]))) 
        }
        helper(ans, res.col = names(ans)[1], 
            start.col = 'start', end.col='end', 
            res.colors = plot.colors, 
            xlab = units, box.color = box.color,
            title = paste("Speech Duration (", units, ")", 
                sep = ""), y2 = y2) 
        mtext(names(ans)[1], side = 2, padj = -4.5)
        if (sums) mtext("sums", side = 4, padj = 1)
    }
    if (is.list(g) & length(g)>1){     
        X <- as.data.frame(ans[, 1], drop = FALSE)   
        names(X) <- names(ans)[1]      
        splits <- colSplit(ans[, 1, drop = FALSE])          
        ans <- data.frame(splits, ans, stringsAsFactors = FALSE, 
            check.names =  FALSE) 
    }  
    if (length(as.data.frame(g))==1){
        ans[, 1] <- as.factor(ans[, 1])
    } else {
        ans[, 1:(length(g) + 1)] <- lapply(ans[, 1:(length(g) + 1)], as.factor)
    }
    la <- length(ans)
    ans[, (la-2):la] <- lapply(ans[, (la-2):la], as.numeric)
    comment(ans) <- units
    ans[, c(ncol(ans)-3)] <- factor(ans[, c(ncol(ans)-3)], levels=LEVS2)
    z[, 1] <- factor(z[, 1], levels=LEVS2)
    if (col.sep != "&") {
        colnames(ans) <- gsub("&", col.sep, colnames(ans), fixed = TRUE)
    }
    if (sums) {
        list("sums" = z, "gantt.df" = ans) 
    } else {
        ans
    }
}