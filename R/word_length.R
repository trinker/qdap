#' Count of Word Lengths Type
#' 
#' Transcript apply word length counts.
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param zero.replace Value to replace 0 values with.
#' @param digits Integer; number of decimal places to round when printing.   
#' @param \ldots Other arguments passed to \code{\link[qdap]{bag_o_words}}.
#' @return Returns a list of:
#' \item{count}{Dataframe of word length counts by grouping variable(s).}
#' \item{prop}{Dataframe of the proportions of word length counts by 
#' grouping variable.} 
#' \item{rnp}{Dataframe of the frequency and proportions of word length counts by 
#' grouping variable.} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @keywords letters
#' @export 
#' @examples
#' \dontrun{
#' (x <- with(DATA, word_length(state, person)))
#' plot(x)
#' scores(x)
#' proportions(x)
#' counts(x)
#' plot(scores(x))
#' plot(proportions(x))
#' plot(counts(x))
#'
#' (x2 <- word_length(DATA[["state"]]))
#' (x2 <- word_length(DATA[["state"]], apostrophe.remove=TRUE))
#'
#' ## Example Visualizations with Presidential Debate Data
#' library(tidyr)
#' (x_long <- proportions(x) %>% 
#'     gather("Letter_Length", "Proportion", -c(1:2)))
#' ggplot(x_long, aes(x = Letter_Length, y = Proportion, color=person, group=person)) +
#'     geom_line(size=.8)
#' 
#' 
#' (x3 <- with(pres_debates2012, word_length(dialogue, person)))
#' (x_long2 <- proportions(x3) %>% 
#'     gather("Letter_Length", "Proportion", -c(1:2)))
#' ggplot(x_long, aes(x = Letter_Length, weight = Proportion, fill=person, group=person)) +
#'     geom_bar()
#' 
#' ggplot(x_long, aes(x = Letter_Length, weight = Proportion, fill=person)) +
#'     geom_bar() + 
#'     facet_wrap(~person, ncol=1)
#'         
#' ggplot(x_long, aes(x = Letter_Length, weight = Proportion, fill=person)) +
#'     geom_bar() + 
#'     coord_flip() + 
#'     facet_wrap(~person, ncol=1)
#' 
#' ggplot(x_long, aes(x = person, weight = Proportion)) +
#'     geom_bar(fill="grey40") + 
#'     coord_flip() + 
#'     facet_grid(Letter_Length~.)
#' }
word_length <- function(text.var, grouping.var = NULL,
    percent = TRUE, zero.replace = 0, digits = 2, ...) {

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF <- data.frame(grouping, text.var = as.character(text.var), check.names = FALSE, 
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))
    DF[, "grouping"] <- factor(DF[, "grouping"])

    words <- lapply(split(DF[["text.var"]] , DF[["grouping"]]), bag_o_words, ...)
    counts <- lapply(words, function(x) nchar(x))

    DF2 <- qdapTools::matrix2df(qdapTools::mtabulate(counts), G)

    DF2 <- data.frame(
        DF2[, 1, drop=FALSE], 
        word.count = rowSums(DF2[-1]), 
        DF2[, -1, drop=FALSE], 
        check.names=FALSE
    )

    DF3 <- data.frame(
        DF2[, 1:2, drop=FALSE], 
        DF2[, -c(1:2), drop=FALSE]/DF2[["word.count"]], 
        check.names=FALSE
    )

    rnp <- raw_pro_comb(DF2[, -c(1:2)], DF3[, -c(1:2)], digits = digits, 
        percent = percent, zero.replace = zero.replace)  
    rnp <- data.frame(DF2[, 1:2], rnp, check.names = FALSE) 
    o <- list(count = DF2, prop = DF3, rnp = rnp, 
        percent = percent, zero.replace = zero.replace, digits = digits)
    class(o) <- "word_length"
    o
}



#' Prints a word_length object
#' 
#' Prints a word_length object
#' 
#' @param x The word_length object
#' @param \ldots ignored
#' @export
#' @method print word_length
print.word_length <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    print(x[["rnp"]])
    options(width=WD)
}

#' Plots a word_length Object
#' 
#' Plots a word_length object.
#' 
#' @param x The word_length object.
#' @param label logical.  If TRUE the cells of the heat map plot will be labeled 
#' with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is TRUE.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proportion.  If NULL uses the value from 
#' \code{\link[qdap]{word_length}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{word_length}}.  Only used if \code{label} is TRUE.
#' @param \ldots Other arguments passed to qheat.
#' @method plot word_length
#' @export
plot.word_length <- function(x, label = FALSE, lab.digits = 1, percent = NULL, 
    zero.replace = NULL, ...) {
    if (label) {
        if (!is.null(percent)) {
            if (percent != x[["percent"]]) {
                DF <- as.matrix(x[["prop"]][, -c(1:2)])
                if (percent) {
                    DF <- DF*100    
                } else {
                    DF <-  DF/100
                }
                x[["prop"]] <- data.frame(x[["prop"]][, 1:2], DF, check.names = FALSE) 
            }
        } else {
            percent <- x[["percent"]]
        }
        if (is.null(zero.replace)) {
            zero.replace <- x[["zero.replace"]]
        }
        rnp <- raw_pro_comb(x[["count"]][, -c(1:2), drop = FALSE], 
            x[["prop"]][, -c(1:2), drop = FALSE], digits = lab.digits, 
            percent = percent, zero.replace = zero.replace)  
        rnp <- data.frame(x[["count"]][, 1:2], rnp, check.names = FALSE) 
 
        ## rename columns
        cnms <- colnames(x[["prop"]])[-c(1:2)]
        colnames(x[["prop"]])[-c(1:2)] <- paste(cnms, ifelse(cnms == "1", "Letter", "Letters"))

        qheat(x[["prop"]], values=TRUE, mat2 = rnp, ...)
    } else {

        ## rename columns
        cnms <- colnames(x[["prop"]])[-c(1:2)]
        colnames(x[["prop"]])[-c(1:2)] <- paste(cnms, ifelse(cnms == "1", "Letter", "Letters"))

        qheat(x[["prop"]], ...)  
    }
}

#' Word Length Counts
#' 
#' View word_length scores.
#' 
#' word_length Method for scores
#' @param x The \code{\link[qdap]{word_length}} object.
#' @param \ldots ignored
#' @export
#' @method scores word_length
scores.word_length <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "word_length_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Word Length Counts
#' 
#' View word_length counts.
#' 
#' word_length Method for counts
#' @param x The \code{\link[qdap]{word_length}} object.
#' @param \ldots ignored
#' @export
#' @method counts word_length
counts.word_length <- function(x, ...) {

    out <- x[["count"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "word_length_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Word Length Counts
#' 
#' View \code{\link[qdap]{word_length}} proportions.
#' 
#' word_length Method for proportions
#' @param x The word_length object.
#' @param \ldots ignored
#' @export
#' @method proportions word_length
proportions.word_length <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "word_length_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



