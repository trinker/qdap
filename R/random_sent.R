#' Generate Random Sentence Samples
#' 
#' Generates a random sample of sentences (sentences are sampled at the word 
#' level and there for are likely nonsensical).
#' 
#' @param n Number of sentences to create.
#' @param len Average length of sentences (in words).
#' @param range Range around \code{len} that number of words may vary.  This may 
#' be a recycled single integer vector or an integer vector of length 2.
#' @param dictionary A dictionary of words to sample from.
#' @param endmark.fun A function to create random end marks. 
#' @return Returns a random vector of sentence strings.
#' @keywords sample random sentence
#' @export
#' @examples
#' \dontrun{
#' random_sent()
#' random_sent(200, 10)
#' 
#' dict <- sort(unique(bag_o_words(pres_debates2012[["dialogue"]])))
#' random_sent(dictionary=dict)
#' }
random_sent <- function(n =10, len = 14, range = len - 1, 
    dictionary = qdapDictionaries::Top200Words, 
    endmark.fun = function() sample(c(".", "!", "|", "?"), 1, 
        prob=c(.85, .05, .05,  .05))){

    x <- seq(len - tail(range, 1), len + head(range, 1), by = 1)
    lens <- replicate(n, max(1, sample(x, 1)))

    sapply(lens, function(x) {
        Caps(paste0(
            unbag(sample(dictionary, x, replace=TRUE)), 
            endmark.fun()
        ))
    })
}


