#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
gantt.plot <-
function(text.var, grouping.var, plot = TRUE, units = "words", 
    sums = FALSE, plot.colors = NULL, box.color = NULL){
    g <- grouping.var
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    grouping.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
        apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))) {
                    NA 
                } else {
                    paste(x, collapse = ".") 
                }
            }
        )
    } else {
        grouping.var
    }
    DF <- data.frame(text = as.character(text.var), 
        group = as.factor(grouping.var), stringsAsFactors = FALSE)
    names(DF) <- c("text", "group")
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
    rownames(z) <- 1:nrow(z)
    if (plot) {
        if (is.null(box.color)) box.color <- "white" 
        y2 <- NULL
        if(sums) y2 <- z[, 2] 
        if (is.null(plot.colors)) {
            plot.colors <- rainbow(10 + length(levels(ans[, 1]))) 
            helper(ans, res.col = names(ans)[1], 
                start.col = 'start', end.col='end', 
                res.colors = plot.colors, 
                xlab = units, box.color = box.color,
                title = paste("Speech Duration (", units, ")", 
                    sep = ""), y2 = y2) 
            mtext(names(ans)[1], side = 2, padj = -4.5)
        }
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
    if (sums) list("sums" = z, "gantt.df" = ans) else return(ans)
}
