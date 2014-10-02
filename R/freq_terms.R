#' Find Frequent Terms
#' 
#' Find the most frequently occurring terms in a text vector.
#' 
#' @param text.var The text variable.
#' @param top Top number of terms to show.
#' @param at.least An integer indicating at least how many letters a word 
#' must be to be included in the output.
#' @param stopwords A character vector of words to remove from the text.  qdap 
#' has a number of data sets that can be used as stop words including: 
#' \code{Top200Words}, \code{Top100Words}, \code{Top25Words}.  For the tm 
#' package's traditional English stop words use \code{tm::stopwords("english")}.
#' @param extend logical.  If \code{TRUE} the \code{top} argument is extended to 
#' any word that has the same frequency as the \code{top} word.
#' @param \ldots Other arguments passed to \code{\link[qdap]{all_words}}.
#' @return Returns a dataframe with the top occurring words.
#' @keywords frequent_terms
#' @export
#' @seealso \code{\link[qdap]{word_list}},
#' \code{\link[qdap]{all_words}}
#' @examples
#' \dontrun{
#' freq_terms(DATA$state, 5)
#' freq_terms(DATA$state)
#' freq_terms(DATA$state, extend = FALSE)
#' freq_terms(DATA$state, at.least = 4)
#' (out <- freq_terms(pres_debates2012$dialogue, stopwords = Top200Words))
#' plot(out)
#' 
#' ## All words by sentence (row)
#' library(qdapTools)
#' x <- raj$dialogue
#' list_df2df(setNames(lapply(x, freq_terms, top=Inf), seq_along(x)), "row")
#' list_df2df(setNames(lapply(x, freq_terms, top=10, stopwords = Dolch), 
#'     seq_along(x)), "Title")
#' 
#' 
#' ## All words by person
#' FUN <- function(x, n=Inf) freq_terms(paste(x, collapse=" "), top=n)
#' list_df2df(lapply(split(x, raj$person), FUN), "person")
#' 
#' ## Plot it
#' out <- lapply(split(x, raj$person), FUN, n=10)
#' pdf("Freq Terms by Person.pdf", width=13) 
#' lapply(seq_along(out), function(i) {
#'     ## dev.new()
#'     plot(out[[i]], plot=FALSE) + ggtitle(names(out)[i])
#' })
#' dev.off()
#' 
#' ## Keep spaces
#' freq_terms(space_fill(DATA$state, "are you"), 500, char.keep="~~")
#' }
freq_terms <- 
function(text.var, top = 20, at.least = 1, stopwords = NULL, 
    extend = TRUE, ...) {

    out <- all_words(text.var, alphabetical = FALSE, ...)
    if (!is.null(stopwords)) {
        out <- out[!out[, "WORD"] %in% tolower(stopwords), ]
    }
    if (!is.null(at.least)) {
        out <- out[nchar(gsub("'", "", out[, "WORD"])) > (at.least - 1), ]
    }

    if (nrow(out) < top) {
        grab <- seq_len(nrow(out))
    } else {
        if (extend) {
            grab <- out[, "FREQ"] >=  out[top, "FREQ"]
        } else {
            grab <- seq_len(top)
        }
    }

    o <- out[grab, ]
    class(o) <- c("freq_terms", class(o))
    o
}

#' Plots a freq_terms Object
#' 
#' Plots a freq_terms object.
#' 
#' @param x The freq_terms object.
#' @param \ldots ignored.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @method plot freq_terms
#' @export
#' @importFrom ggplot2 ggplot aes_string  geom_bar ylab xlab scale_y_continuous theme element_blank theme_bw
plot.freq_terms <- function(x, plot = TRUE, ...) {

    x[, "WORD"] <- factor(x[, "WORD"], levels = rev(x[, "WORD"]))
    mx <- max(x[, "FREQ"])

    GP <- ggplot(x, aes_string(x="WORD")) +
        geom_bar(aes_string(weight="FREQ")) +
        coord_flip() + ylab("Count") + xlab("Word") +
        scale_y_continuous(expand = c(0,0), 
           limits = c(0, mx + (.03 * mx))) +
        theme_qdap() 

    if (plot) {
        print(GP)
    }
    invisible(GP)
}


