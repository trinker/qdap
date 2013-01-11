#' Stem Text
#' 
#' \code{stemmer} - Stems a vector of text strings.
#' 
#' @param text.var  The text variable.  In \code{stemmer} this is a vector text 
#' string. For \code{stem2df} this is a character vector of length one naming the 
#' text column.  
#' @param rm.bracket logical.  If TRUE brackets are removed from the text.
#' @param capitalize logical.  If TRUE selected terms are capitalized
#' @param warn logical.  If TRUE warns about rows not ending with standard qdap 
#' punctuation endmarks.
#' @param \dots Various: \cr
#'     \emph{\code{stemmer} - Other arguments passed to 
#'     \code{\link[qdap]{capitalizer}}} \cr
#'     \emph{\code{stem.words} - Words or terms.} \cr
#'     \emph{\code{stem2df} - Other arguments passed to 
#'     \code{\link[qdap]{stemmer}}}
#' @param dataframe A dataframe object.
#' @param stem.name A character vector of length one for the stemmed column.  If 
#' \code{NULL} defaults to \code{"stem.text"}.
#' @return \code{stemmer } - returns a character vector with stemmed text.
#' @rdname stemmer
#' @seealso \code{\link[qdap]{capitalizer}}
#' @keywords stem
#' @examples
#' \dontrun{
#' #stemmer EXAMPLE:
#' stemmer(DATA$state)
#' stemmer(raj$dialogue)
#' 
#' #stem.words EXAMPLE:
#' stem.words(doggies, jumping, swims)
#' 
#' #stem2df EXAMPLE:
#' stem2df(DATA, "state", "new")
#' }
stemmer <-
function(text.var, rm.bracket = TRUE, capitalize = TRUE, warn = TRUE, ...){
    txt <- as.character(text.var)
    if (rm.bracket){
        txt <- bracketX(txt)
    }
    nc <- nchar(txt)
    end.mark1 <- substring(txt, nc, nc)
    end.mark2 <- substring(txt, nc - 1, nc)
    sent.type <- ifelse(end.mark2 %in% c("*.", "*?", "*!", "*|"), end.mark2,
        ifelse(end.mark1 %in% c(".", "?", "!", "|"), end.mark1,    
        ifelse(is.na(end.mark1), NA, "")))
    bl <- sent.type == ""
    if (any(na.omit(bl)) & warn) {
        warning(paste(
            "The following row(s) do have standard qdap punctuation endmarks:\n", 
            " rows:", paste(which(bl), collapse = ", "), "\n"))
    }
    LIST <- qdap::stopwords(txt, stopwords = NULL, strip = TRUE)
    LIST <- lapply(LIST, function(x) {
        if(identical(x, character(0))) {
                NA
            } else {
                tm::stemDocument(x)
            }
        }
    )
    if (capitalize){
        LIST <- lapply(LIST, function(x) capitalizer(x, ...))
    }
    if (length(text.var) == 1) {
        if (!is.na(text.var)) {
            LIST <- paste2(LIST, sep=" ")
        }
    }
    LIST2 <- lapply(LIST, function(x) paste(x, collapse=" "))
    txt2 <- paste2(list(LIST2, sent.type), sep="")
    if (capitalize){
        capit <- function (string) {
            if (string == "NA" | is.na(string)) {
                NA
            } else {
                nc <- nchar(string)
                paste0(toupper(substr(string, 1, 1)), substr(string, 2, nc))
            }
        }
    txt2 <- unlist(lapply(Trim(txt2), capit))
    }
    return(txt2)
}

#' Stem Words
#' 
#' \code{stem.words} - Wrapper for stemmer that stems a vector of words.
#' 
#' @return \code{stem.words} - returns a dataframe with a character vector with.
#' 
#' @rdname stemmer
#' @export
stem.words <- 
function(...) {
    x <- substitute(...())
    z <- unblanker(scrubber(unlist(lapply(x, function(y) as.character(y)))))
    stemmer(z, capitalize = FALSE, warn = FALSE)
}

#' Stem Text
#' 
#' \code{stem2df} - Wrapper for stemmer that stems a vector of text strings 
#' and returns a dataframe with the vector added..
#' 
#' @return \code{stem2df} - returns a dataframe with a character vector with 
#' stemmed text.
#' @rdname stemmer
#' @export
stem2df <-
function (dataframe, text.var, stem.name = NULL, ...) {
    if (!is.data.frame(dataframe)) {
        stop("Please supply a data.frame to stem2df")
    }
    if (is.numeric(dataframe[, text.var])) {
        stop("text.var can not be numeric")
    }
    new <- stemmer(dataframe[, text.var], ...)
    DF <- data.frame(dataframe, stem.text = new, stringsAsFactors = FALSE)
    if (!is.null(stem.name)) {
        names(DF)[ncol(DF)] <- stem.name
    } 
    return(DF)
}
