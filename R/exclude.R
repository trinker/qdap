#' Exclude Elements From a Vector
#' 
#' \code{exclude} - Quickly exclude words from a word list
#' 
#' @param word.list A list/vector of words/terms, a \code{\link[qdap]{wfm}}, 
#' \code{\link[tm]{DocumentTermMatrix}}, or \code{\link[tm]{TermDocumentMatrix}}
#' to exclude from.
#' @param \dots A vector (character/numeric) if element(s) to be excluded from 
#' the \code{word.list}.
#' @return Returns a vector with the excluded terms removed.
#' @export
#' @rdname exclude
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
#' dat2 <- as.dtm(DATA$state, DATA$person)
#' out.dtm <- exclude(dat2, unlist(the.no))
#' tm::inspect(out.dtm)
#' 
#' dat3 <- as.tdm(DATA$state, DATA$person)
#' out.tdm <- exclude(dat3, unlist(the.no))
#' tm::inspect(out.tdm)
#' }
exclude <-
function(word.list, ...){
    word.list
    UseMethod("exclude")
}

#' \code{exclude.TermDocumentMatrix} - TermDocumentMatrix method for \code{exclude}.
#' @rdname exclude
#' @export
#' @method exclude TermDocumentMatrix    
exclude.TermDocumentMatrix <-
function(word.list, ...) {

    mes <- try(is.vector(...), TRUE)
    if(!inherits(mes, "try-error")) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }

    word.list[!rownames(word.list) %in% excluded,]
}

#' \code{exclude.DocumentTermMatrix} - DocumentTermMatrix method for \code{exclude}.
#' @rdname exclude
#' @export
#' @method exclude DocumentTermMatrix    
exclude.DocumentTermMatrix <-
function(word.list, ...) {

    mes <- try(is.vector(...), TRUE)
    if(!inherits(mes, "try-error")) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }

    word.list[, !colnames(word.list) %in% excluded]
}

#' \code{exclude.wfm} - wfm method for \code{exclude}.
#' @rdname exclude
#' @export
#' @method exclude wfm    
exclude.wfm <-
function(word.list, ...) {

    mes <- try(is.vector(...), TRUE)
    if(!inherits(mes, "try-error")) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }
  
    z <- word.list[!rownames(word.list) %in% excluded,]
    class(z) <- class(word.list) 
    z
}


#' \code{exclude.list} - list method for \code{exclude}.
#' @rdname exclude
#' @export
#' @method exclude list    
exclude.list <-
function(word.list, ...) {

    mes <- try(is.vector(...), TRUE)
    if(!inherits(mes, "try-error")) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }

    lapply(word.list, function(x) x[!x %in% excluded])
}

#' \code{exclude.default} - default method for \code{exclude}.
#' @rdname exclude
#' @export
#' @method exclude default    
exclude.default <-
function(word.list, ...) {

    mes <- try(is.vector(...), TRUE)
    if(!inherits(mes, "try-error")) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }

    word.list[!word.list %in% excluded]
}


#' Exclude Elements From a Vector
#' 
#' \code{\%ex\%} - Binary operator version of \code{\link[qdap]{exclude}} .
#' 
#' @rdname exclude
#' @export
`%ex%` <- function(word.list, ...) {
    exclude(word.list=word.list, ...)
}

