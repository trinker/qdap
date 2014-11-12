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
#' topwords <- freq_terms(text.var, top = top, at.least = at.least, 
#'     stopwords = stopwords)[[1]]
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
#' plots <- lapply(names(out2), function(x) plot(out2[[x]]) + ggtitle(x))
#' do.call("grid.arrange", c(plots, ncol=2))
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
     
        out <- na.omit(unlist(sapply(match.terms, function(y){
            out <- which(y == x)
            if (identical(out, integer(0))) out <- NA
            out
        })))

        setNames(out, gsub("\\d", "", names(out)))
    })
    
    ## unlist postions for later count    
    position2 <- unlist(position)
   
    dat <- data.frame(word=names(position2), position=unname(position2))
    
    Freq <- df2matrix <- NULL    
    
    dat2b <- table(dat)
    dat2 <- data.frame(dat2b) %>% tidyr::spread(position, Freq)

    inds <- suppress_plot(heatmap(as.matrix(dat2b)), 
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
    pdf(file.path(tdir, "out.pdf"))
    out <- eval(substitute(...())[[1]], envir = envir)
    dev.off()
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
        heatmap(t(qdapTools::df2matrix(dat)), Colv=NA, ylab="Word", 
            xlab="Position", scale=if( scale){"row"}else{"none"}, ...)
    } else {
        out <- qheat(dat, high="blue", low="yellow", 
                by.column=if( scale){ FALSE}else{ NULL}, 
                digits=3, plot=FALSE, grid=NULL, ...) +
          ylab("Word") + xlab("Position") + 
          guides(fill=guide_legend(title="Proportion"))
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
    print(plot(x, ...))
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



