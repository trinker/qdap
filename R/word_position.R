#' Word Position
#' 
#' Find counts of the positioning of words within a sentence.
#' 
#' @param text.var The text variable.
#' @param match.terms  A character vector of quoted terms to find the positions of.  
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param digits Integer; number of decimal places to round when printing.   
#' @param zero.replace Value to replace 0 values with.
#' @param \ldots Currently ignored.
#' @return Returns a list, of class "word_position", of data frames and 
#' information regarding word positions:
#' \item{raw}{raw word position counts in long format (may be more useful for plotting)} 
#' \item{count}{integer word position counts} 
#' \item{prop}{proportional word position counts; proportional to 
#' each total word uses} 
#' \item{rnp}{a character combination data frame of count and proportional}     
#' \item{zero_replace}{value to replace zeros with; mostly internal use}   
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{digits}{integer value of number of digits to display; mostly internal 
#' use}   
#' @note Default printing is a heatmap plot.
#' @keywords word position
#' @export
#' @examples
#' \dontrun{
#' position <- with(DATA, word_position(sent_detect(state), Top25Words))
#' position
#' lview(position)
#' plot(position)
#' scores(position)
#' preprocessed(position)
#' counts(position)
#' proportions(position)
#' plot(proportions(position))
#' 
#' stopwords <- unique(c(contractions[[1]], Top200Words))
#' topwords <- freq_terms(pres_debates2012[["dialogue"]], top = 40, 
#'     at.least = 4, stopwords = stopwords)[[1]]
#' word_position(pres_debates2012[["dialogue"]], topwords)
#' plot(word_position(pres_debates2012[["dialogue"]], topwords), FALSE)
#' plot(word_position(pres_debates2012[["dialogue"]], topwords), TRUE, scale=FALSE)
#' 
#' wordlist <- c("tax", "health", "rich", "america", "truth", "money", "cost", 
#'     "governnor", "president", "we", "job", "i", "you", "because", 
#'     "our", "years")
#' 
#' word_position(pres_debates2012[["dialogue"]], wordlist)
#'
#' ## BY VARIABLES
#' library(gridExtra)
#' pres_deb_by_time <- with(pres_debates2012, split(dialogue, time))
#' out1 <-lapply(pres_deb_by_time, word_position, wordlist)
#' do.call("grid.arrange", c(lapply(out1, plot), ncol=1))
#' 
#' pres_deb_by_person <- with(pres_debates2012, split(dialogue, person))
#' out2 <-lapply(pres_deb_by_person, word_position, wordlist)
#' plots <- lapply(names(out2), function(x) plot(out2[[x]], scale=FALSE) + 
#'     ggtitle(x))
#' do.call("grid.arrange", c(plots, ncol=2))
#' 
#' ## As a histogram
#' ## theme taken from: http://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2/
#' theme_black <- function(base_size=12,base_family="") {
#'   theme_grey(base_size=base_size,base_family=base_family) %+replace%
#'     theme(
#'       # Specify axis options
#'       axis.line=element_blank(), 
#'       axis.text.x=element_text(size=base_size*0.8,color="grey55",
#'                                lineheight=0.9,vjust=1), 
#'       axis.text.y=element_text(size=base_size*0.8,color="grey55",
#'                                lineheight=0.9,hjust=1), 
#'       axis.ticks=element_line(color="grey55",size = 0.2), 
#'       axis.title.x=element_text(size=base_size,color="grey55",vjust=1), 
#'       axis.title.y=element_text(size=base_size,color="grey55",angle=90,
#'                                 vjust=0.5), 
#'       axis.ticks.length=unit(0.3,"lines"), 
#'       axis.ticks.margin=unit(0.5,"lines"),
#'       # Specify legend options
#'       legend.background=element_rect(color=NA,fill="black"), 
#'       legend.key=element_rect(color="grey55", fill="black"), 
#'       legend.key.size=unit(1.2,"lines"), 
#'       legend.key.height=NULL, 
#'       legend.key.width=NULL,     
#'       legend.text=element_text(size=base_size*0.8,color="grey55"), 
#'       legend.title=element_text(size=base_size*0.8,face="bold",hjust=0,
#'                                 color="grey55"), 
#'       legend.position="right", 
#'       legend.text.align=NULL, 
#'       legend.title.align=NULL, 
#'       legend.direction="vertical", 
#'       legend.box=NULL,
#'       # Specify panel options
#'       panel.background=element_rect(fill="black",color = NA), 
#'       panel.border=element_rect(fill=NA,color="grey55"), 
#'       panel.grid.major=element_blank(), 
#'       panel.grid.minor=element_blank(), 
#'       panel.spacing=unit(0.25,"lines"),  
#'       # Specify facetting options
#'       strip.background=element_rect(fill="grey30",color="grey10"), 
#'       strip.text.x=element_text(size=base_size*0.8,color="grey55"), 
#'       strip.text.y=element_text(size=base_size*0.8,color="grey55",
#'                                 angle=-90), 
#'       # Specify plot options
#'       plot.background=element_rect(color="black",fill="black"), 
#'       plot.title=element_text(size=base_size*1.2,color="grey55"), 
#'       plot.margin=unit(c(1,1,0.5,0.5),"lines")
#'     )
#' }
#' 
#' out3 <- list_df2df(lapply(out2[1:2], preprocessed), "Person")
#' out3 %>% ggplot(aes(x=position)) + 
#'     geom_histogram(binwidth = 1, fill="white") +
#'     facet_grid(Person~word) +
#'     theme_black() + ylab("Count") + xlab("Position")
#' 
#' ## MOVE TO THE MICRO THROUGH QUALITATIVE ANALYSIS
#' locs <- unlist(setNames(lapply(wordlist, function(x){
#'      sapply(c("ROMNEY", "OBAMA"), function(y){
#'          which(pres_debates2012[["person"]] ==y & grepl(x, pres_debates2012[["dialogue"]]))
#'      })
#' }), wordlist), recursive=FALSE)
#' 
#' fdl <- qdap:::folder(pres_context)
#' Map(function(x, y){
#'     if (identical(integer(0), x)) return(NULL)
#'     z <- with(pres_debates2012, trans_context(dialogue, person, inds=x, n.before=1))
#'     z[["text"]] <- gsub(beg2char(y, "."), 
#'         paste0("[[", beg2char(y, "."), "]]"), z[["text"]])
#'     print(z, file=file.path(fdl, sprintf("%s.doc", y)))
#' }, locs, names(locs))
#' }
word_position <- function(text.var, match.terms, digits = 2, 
    percent = TRUE, zero.replace = 0, ...){

    ## make sure text.var is character with no double punctuation
    text.var <- as.character(text.var)
    if (is.dp(text.var = text.var)) {
        warning(paste0("\n  `text.var` contains double punctuation.", 
            "  Suggested use of `sentSplit` or `sent_detect` functions."))
    }

    ## split sentences into bag of words and find positioning of match.terms
    position <- lapply(lapply(text.var, bag_o_words), function(x){
     
        out <- stats::na.omit(unlist(sapply(match.terms, function(y){
            out <- which(y == x)
            if (identical(out, integer(0))) out <- NA
            out
        })))

        stats::setNames(out, gsub("\\d", "", names(out)))
    })
    
    ## unlist postions for later count    
    position2 <- unlist(position)
   
    dat <- data.frame(word=names(position2), position=unname(position2))
    
    Freq <- df2matrix <- NULL    
    
    dat2b <- table(dat)
    dat2 <- data.frame(dat2b) %>% tidyr::spread(position, Freq)

    inds <- suppress_plot(stats::heatmap(as.matrix(dat2b)), 
        envir = environment())[["rowInd"]]
    dat2[["word"]] <- factor(dat2[["word"]], 
        levels = as.character(dat2[["word"]])[inds])
    
    len <- length(position2)
    dat2c <- dat2
    dat2c[-1] <- lapply(dat2[-1], function(x) x/len)
      
    dat4 <- matrix2df(t(df2matrix(dat2c)[inds, ]), "Position")
    dat4[["Position"]] <- factor(dat4[["Position"]], levels=dat4[["Position"]])

    dat5 <- matrix2df(t(df2matrix(dat2)[inds, ]), "Position")
    dat5[["Position"]] <- factor(dat5[["Position"]], levels=dat5[["Position"]])


    dat6 <- raw_pro_comb(dat5[, -1], dat4[, -1], digits=digits, percent=percent, 
        zero.replace=zero.replace)
    dat6 <- data.frame(dat5[, 1, drop=FALSE], dat6)
    o <- list(raw = dat, count = dat5, prop = dat4, rnp = dat6, 
        zero.replace = zero.replace, percent = percent, digits = digits)

    class(o) <- "word_position"
    o
}

## helper function
suppress_plot <- function(..., envir = envir) {
    tdir <- tempdir()
    grDevices::pdf(file.path(tdir, "out.pdf"))
    out <- eval(substitute(...())[[1]], envir = envir)
    grDevices::dev.off()
    out
}

#' Plots a word_position object
#' 
#' Plots a word_position object.
#' 
#' @param x The word_position object.
#' @param qheat logical.  If \code{TRUE} \code{\link[qdap]{qheat}} is used to 
#' plot.  If \code{FALSE} \code{\link[stats]{heatmap}} is used.
#' @param scale logical.  If \code{TRUE} scales heatmap by row.  If 
#' \code{FALSE} no scaling occurs.
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}} or 
#' \code{\link[stats]{heatmap}}.
#' @method plot word_position
#' @export
plot.word_position <- function(x, qheat = TRUE, scale = TRUE, ...){

    dat <- x[["prop"]]

    if (!qheat){
        stats::heatmap(t(qdapTools::df2matrix(dat)), Colv=NA, ylab="Word", 
            xlab="Position", scale=if( scale){"row"}else{"none"}, ...)
    } else {
        out <- qheat(dat, high="blue", low="yellow", 
                by.column=if( scale){ FALSE}else{ NULL}, 
                digits=3, plot=FALSE, grid=NULL, ...) +
            ggplot2::ylab("Word") + 
            ggplot2::xlab("Position") + 
            ggplot2::guides(fill=ggplot2::guide_legend(title="Proportion"))
        out
    }
}

#' Prints a word_position object.
#' 
#' Prints a word_position object.
#' 
#' @param x The word_position object
#' @param \ldots Values passed to \code{plot.word_position}
#' @export
#' @method print word_position
print.word_position <- function(x, ...){
    print(graphics::plot(x, ...))
}


#' Word Position
#' 
#' View word_position scores.
#' 
#' word_position Method for scores
#' @param x The \code{\link[qdap]{word_position}} object.
#' @param \ldots ignored
#' @export
#' @method scores word_position
scores.word_position <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "word_position_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Word Position
#' 
#' View word_position counts.
#' 
#' word_position Method for counts
#' @param x The \code{\link[qdap]{word_position}} object.
#' @param \ldots ignored
#' @export
#' @method counts word_position
counts.word_position <- function(x, ...) {

    out <- x[["count"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "word_position_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Word Position
#' 
#' View \code{\link[qdap]{word_position}} proportions.
#' 
#' word_position Method for proportions
#' @param x The word_position object.
#' @param \ldots ignored
#' @export
#' @method proportions word_position
proportions.word_position <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "word_position_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



#' Word Position
#' 
#' View \code{\link[qdap]{word_position}} preprocessed.
#' 
#' word_position Method for preprocessed
#' @param x The word_position object.
#' @param \ldots ignored
#' @export
#' @method preprocessed word_position
preprocessed.word_position <- function(x, ...) {

    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "word_position_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



