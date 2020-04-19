#' Gantt Durations
#' 
#' \code{gantt} - Generates start and end times of supplied text selections 
#' (i.e., text selections are determined by any number of grouping variables).
#' 
#' @param text.var The text variable    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param units The unit of measurement to analyze.  One of the strings 
#' \code{"character"}, \code{"syllable"}, \code{"word"}, or \code{"sentence"}.
#' @param sums logical.  If \code{TRUE} reports and (optionally (or plots) the 
#' total units used by grouping variable(s).
#' @param col.sep The character string to use to separate pasted variables in 
#' the merged grouping variable header/name.
#' @return Returns a data frame of start and end times by grouping variable(s) 
#' or optionally returns a list of two: (1) A data frame of the total units 
#' used by grouping variable(s) and (2) a data frame of start and end times 
#' by grouping variable(s).  
#' @note For non-repeated measures data use \code{\link[qdap]{gantt}}.  For
#' more flexible plotting needs use \code{\link[qdap]{gantt_wrap}} over the 
#' generic plotting method.
#' @author DigEmAll (stackoverflow.com) and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso \code{\link[qdap]{gantt_rep}},
#' \code{\link[qdap]{gantt_wrap}},
#' \code{\link[qdap]{gantt_plot}} 
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @rdname gantt
#' @examples
#' \dontrun{
#' (a <- gantt(DATA$state, DATA$person))  
#' plot(a)
#' plot(a, base = TRUE)
#' 
#' (b <- gantt(DATA$state, DATA$person, sums = TRUE)) 
#' plot(b)
#' plot(b, base = FALSE) 
#' 
#' (d <- gantt(DATA$state, list(DATA$sex, DATA$adult)))        
#' plot(d)
#' 
#' x <- gantt(mraja1$dialogue, mraja1$person) 
#' plot(x, base = TRUE)
#' plot(x, , base = TRUE, box.color = "black") 
#' 
#' z <- gantt(mraja1$dialogue, mraja1$sex)  
#' plot(z)  
#'                                                           
#' e <- with(mraja1, gantt(dialogue, list(fam.aff, sex, died), 
#'    units = "characters", sums = TRUE))
#' plot(e)  
#'      
#' f <- gantt(mraja1$dialogue, mraja1$person, units = "syllables",
#'     sums = TRUE)
#' plot(f, box.color = "red")
#' plot(f, base = FALSE)
#' 
#' dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
#'     units = "sentences", col.sep = "_")
#'     
#'     
#' ## Animate It
#' ##=================
#' ani_gannt <- with(DATA.SPLIT, gantt(state, person))
#' Animate(ani_gannt)
#' Animate(plot(ani_gannt))
#' 
#' library(animation)
#' loc <- folder(animation_gantt)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN <- function() {
#'     out <- Animate(ani_gannt)
#'     lapply(out, function(x) {
#'         print(x)
#'         animation::ani.pause()
#'     })
#' 
#' }
#' 
#' type <- if(.Platform$OS.type == "windows") shell else system
#' saveGIF(FUN(), interval = 0.1, outdir = loc, cmd.fun = type)
#' }
gantt <-
function(text.var, grouping.var, units = "words", sums = FALSE, col.sep = "_"){
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
        grouping.var <- paste2(grouping.var, sep = col.sep)
    } else {
        grouping.var <- unlist(grouping.var)
    }     
    
##     if (is.list(grouping.var) & length(grouping.var)>1) {
##         grouping.var <- apply(data.frame(grouping.var), 1, function(x){
##                 if (any(is.na(x))) {
##                     NA 
##                 } else {
##                     paste(x, collapse = ".") 
##                 }
##             }
##         )
##     } else {
##         grouping.var <- grouping.var
##     }

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
        syllables = ner <- function(x) syllable_sum(x), 
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
    ans[, c(ncol(ans)-3)] <- factor(ans[, c(ncol(ans)-3)], levels=LEVS2)
    z[, 1] <- factor(z[, 1], levels=LEVS2)
    if (col.sep != "&") {
        colnames(ans) <- gsub("&", col.sep, colnames(ans), fixed = TRUE)
    }
    class(ans) <- c("gantt", paste0("unit_", units), class(ans))
    if (sums) {
        ans <- list("sums" = z, "gantt.df" = ans) 
        class(ans) <- c("sums_gantt", class(ans))
        ans
    } else {
        ans
    }
}

#' Prints a sums_gantt object
#' 
#' Prints a sums_gantt object.
#' 
#' @param x The sums_gantt object 
#' @param \ldots ignored
#' @method print sums_gantt
#' @export
print.sums_gantt <- function(x, ...) {
   class(x) <- "list"
   print(x)
}


#' Generate Unit Spans 
#' 
#' \code{plot_gantt_base} - For internal use.
#' 
#' @param x  n object of the class "gantt".
#' @param fill.colors The colors of the Gantt plot bars.  Either a single color 
#' or a length equal to the number of grouping variable(s).  If \code{NULL}, 
#' \code{rainbow} is used.
#' @param box.color A color to wrap the boxes with.
#' @param title An optional title.
#' @rdname gantt
#' @export
plot_gantt_base <- function(x, sums = NULL, fill.colors = NULL, 
    box.color = "white", title = NULL){
    if(methods::is(x, "sums_gantt")) {
        sums <- x[["sums"]][, 2]
        x <- x[["gantt.df"]]        
    } 
    units <- gsub("unit_", "", class(x)[grepl("unit_", class(x))])
    if (is.null(fill.colors)) {
        fill.colors <- grDevices::rainbow(10 + length(levels(x[, 1]))) 
    }
    helper(data = x, res.col = colnames(x)[1], 
        start.col = 'start', end.col='end', 
        res.colors = fill.colors, 
        xlab = Caps(units), box.color = box.color,
        title = title, 
        y2 = sums) 
    graphics::mtext(Caps(colnames(x)[1]), side = 2, padj = -4.5)
    if (!is.null(sums)) {
        graphics::mtext("Sums", side = 4, padj = 1)
    }
}


#' Plots a gantt object
#' 
#' Plots a gantt object.
#' 
#' @param x The sums_gantt object
#' @param base logical.  If \code{TRUE} prints in base graphics system.  
#' If \code{FALSE} prints in ggplot graphics system.
#' @param title An optional title.
#' @param \ldots Other arguments passed to \code{gantt_wrap} or 
#' \code{plot_gantt_base}
#' @method plot gantt
#' @export
plot.gantt <- function(x, base = FALSE, title = NULL, ...) {

    units <- gsub("unit_", "", class(x)[grepl("unit_", class(x))])
    if (is.null(title)) {
        title <- sprintf("Speech Duration (%s)", Caps(units))
    }
    if (base) {
        plot_gantt_base(x, title = title, ...)
    } else {
        gantt_wrap(x, colnames(x)[1], title = title, ...)  
    }

}

#' Plots a sums_gantt object
#' 
#' Plots a sums_gantt object.
#' 
#' @param x The sums_gantt object
#' @param base logical.  If \code{TRUE} prints in base graphics system.  
#' If \code{FALSE} prints in ggplot graphics system.
#' @param title An optional title.
#' @param \ldots Other arguments passed to \code{gantt_wrap} or 
#' \code{plot_gantt_base}
#' @method plot sums_gantt
#' @export
plot.sums_gantt <- function(x, base = TRUE, title = NULL, ...) {
    units <- gsub("unit_", "", class(x[["gantt.df"]])[grepl("unit_", 
        class(x[["gantt.df"]]))])
    if (is.null(title)) {
        title <- sprintf("Speech Duration (%s)", Caps(units))
    }
    if (base) {
        plot_gantt_base(x, title = title, ...)
    } else {
        x <- x[["gantt.df"]]
        gantt_wrap(x, colnames(x)[1], title = title, ...)  
    }
}

# helper function for gantt (not exported)
helper <-
function(data, res.col = "person", start.col = "start",
    end.col = "end", res.colors = grDevices::rainbow(40), title = NULL, 
    box.color = "black", xlab = "Duration", ylab = NA, y2 = NULL){
    op <- graphics::par("mar")
    if (is.null(y2)){
        graphics::par(mar = op + c(0,3,0,0)) 
    } else {
        graphics::par(mar = op + c(0,3,0,2.2)) 
    }
    on.exit(graphics::par(mar = c(5, 4, 4, 2) + 0.1))
    minval <- min(data[,start.col])
    maxval <- max(data[,end.col])
    res.colors <- rev(res.colors)
    resources <- sort(unique(data[,res.col]),decreasing=T)
    graphics::plot(c(minval,maxval),
       c(0.5,length(resources)+0.5),
       type="n", xlab=xlab,ylab=ylab,yaxt="n" , main = title, 
       cex.main = 1)
    graphics::axis(side=2,at=1:length(resources),labels=resources,las=1)
    if (!is.null(y2)){
        graphics::axis(side=4,at=1:length(y2),labels=rev(y2),las=1) 
    }
    for (i in 1:length(resources)) {
        yTop <- i+0.1
        yBottom <- i-0.1
        subset <- data[data[,res.col] == resources[i],]
        for(r in 1:nrow(subset)) {
            color <- res.colors[((i-1)%%length(res.colors))+1]
            start <- subset[r,start.col]
            end <- subset[r,end.col]
            graphics::rect(start,yBottom,end,yTop,col=color, border= box.color)
        }
    }
}


## Internal helper function to capitalize words
Caps <- function(x, all = FALSE) { 
    if (all) {    
        x <- strsplit(x, " ")[[1]]
    } 
    paste(toupper(substring(x, 1,1)), substring(x, 2), sep="", collapse=" ") 
}



#' Gantt Durations
#' 
#' \code{gantt} - Animate discourse from \code{\link[qdap]{gantt}}.
#' 
#' gantt Method for Animate
#' @param x The gantt object.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param colors An optional character vector of colors to color the Gantt bars.
#' Must be length 1 (repeats the same color) or equal to the levels of the 
#' grouping variable.
#' @param \ldots Other arguments passed to \code{\link[qdap]{gantt_wrap}}.
#' @export
#' @importFrom qdapTools %l%
#' @method Animate gantt
Animate.gantt <- function(x, wc.time = TRUE, time.constant = 2, colors = NULL, ...){

    if (!"n" %in% colnames(x)) stop("x contains no column \"n\"")
    z <- graphics::plot(x,  plot=FALSE) ###
    z[[c("coordinates", "limits", "x")]] <- c(0, sum(x[, "n"]))
       
    plots <- lapply(0:nrow(x), function(i, myplot=z, dat = z[["data"]]) {

        ## Starts the plot with no bars if i = 0
        if (i == 0) {
            thedat <- dat[1, , drop=FALSE]
            thedat[, "end"] <- 0
        } else {
            thedat <- dat[1:i, , drop=FALSE]
        }

        ## Proper coloring
        num <- ifelse(i == 0, 1, i)
        colvar <- thedat[1:num, "new4"]
        lvls <- levels(colvar)
        if (is.null(colors)) {
            lvls_cols <- gg_color_hue(length(lvls))
        } else {
            if (length(colors) == 1) {
                lvls_cols <- rep(colors, length(lvls))
            } else {
                lvls_cols <- colors
            }
        }

        colvars2 <- levels(unique(colvar))[levels(unique(colvar)) %in% unique(colvar)]
        cols <- colvars2 %l% data.frame(lvls, lvls_cols, stringsAsFactors = FALSE)
  
        myplot[["data"]] <- thedat
        myplot + scale_y_discrete(drop=FALSE) + scale_colour_manual(values=cols)

    })
        
    timings <- round(exp(z[["data"]][, "n"]/(max(z[["data"]][, "n"])/time.constant)))
    if(wc.time) {
        plots <- rep(plots, c(1, timings))
    }
    plots
}

