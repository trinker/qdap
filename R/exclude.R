#' Exclude Elements From a Vector
#' 
#' Quickly exclude words from a word list
#' 
#' @param word.list A list/vector of words/terms, a \code{\link[qdap]{wfm}}, 
#' \code{\link[tm]{DocumentTermMatrix}}, or \code{\link[tm]{TermDocumentMatrix}}
#' to exclude from.
#' @param \dots A vector (character/numeric) if element(s) to be excluded from 
#' the \code{word.list}.
#' @return Returns a vector with the excluded terms removed.
#' @export
#' @examples
#' \dontrun{
#' exclude(1:10, 3, 4)
#' exclude(1:10, 3:4)
#' Top25Words
#' exclude(Top25Words, qcv(the, of, and))
#' exclude(Top25Words, "the", "of", "an")
#' 
#' #Using with term_match and termco 
#' terms <- term_match(DATA$state, qcv(th), FALSE) 
#' exclude(terms, "truth")  
#' #all together
#' termco(DATA$state, DATA$person, exclude(term_match(DATA$state, qcv(th), 
#'     FALSE), "truth"))
#' 
#' MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
#' termco(DATA$state, DATA$person, MTCH.LST)
#'
#' ## Works with wfm
#' dat <- wfm(DATA$state, DATA$person)
#' the.no <- term_match(DATA$state, c("the", "no"))
#' exclude(dat, unlist(the.no))
#' 
#' ## Works with tm's TermDocumentMatrix/DocumentTermMatrix
#' dat2 <- dtm(DATA$state, DATA$person)
#' out.dtm <- exclude(dat2, unlist(the.no))
#' tm::inspect(out.dtm)
#' 
#' dat3 <- tdm(DATA$state, DATA$person)
#' out.tdm <- exclude(dat3, unlist(the.no))
#' tm::inspect(out.tdm)
#' }
exclude <-
function(word.list, ...) {
    mes <- try(is.vector(...), TRUE)
    if(substring(mes[[1]], 1, 5) != "Error") {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }

    cls <- class(word.list)
    ## Handling for TermDocumentMatrix/DocumentTermMatrix
    if ("DocumentTermMatrix" %in% cls | "TermDocumentMatrix" %in% cls) {  
        z <- tm2qdap(word.list)
        cls2 <- class(z) 
        z <- z[!rownames(z) %in% excluded,]
        class(z) <- cls2
        if ("DocumentTermMatrix" %in% cls) {
            z <- dtm(z)
        } else {
            z <- tdm(z)
        }
        return(z)
    }

    ## Handling for wfm
    if ("wfm" %in% cls) {  
        z <- word.list[!rownames(word.list) %in% excluded,]
        class(z) <- cls
        return(z)
    }

    if (!is.list(word.list)) {
      word.list[!word.list %in% excluded]
    } else {
      lapply(word.list, function(x) x[!x %in% excluded])
    }
}




