#' Generate ngrams
#' 
#' Transcript apply ngrams.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param n The max number of grams calculate
#' @param \dots Further arguments passed to strip function.
#' @return Returns a list of:
#' \item{raw}{A list of pasted single vectors of the ngrams per row.}
#' \item{group}{A list of pasted vectors of ngrams grouped by grouping.var.}
#' \item{unlist1}{A list of a single vector of pasted ngrams per grouping.var in the order used.} 
#' \item{unlist2}{A list of a single vector of pasted ngrams per grouping.var in alphabetical order.}
#' \item{group_n}{A list of a list of vectors of ngrams per grouping.var & n (not pasted).}
#' \item{all}{A single vector of pasted ngrams sorted alphabetically.}
#' \item{all_n}{A list of lists a single vectors of ngrams sorted alphabetically (not pasted).}
#' @keywords ngram
#' @export 
#' @examples
#' \dontrun{
#' ngrams(DATA$state, DATA$person, 2)
#' ngrams(DATA$state, DATA$person, 3)
#' ngrams(DATA$state, , 3)
#' with(mraja1, ngrams(dialogue, list(sex, fam.aff), 3))
#' 
#' ## Alternative ngram analysis:
#' n_gram <- function(x, n = 2, sep = " "){
#' 
#'     m <- qdap::bag_o_words(x)
#'     if (length(m) < n) return(character(0))
#'     starts <- 1:(length(m) - (n - 1))
#'     ends <- n:length(m) 
#'     Map(function(x, y){
#'             paste(m[x:y], collapse=sep)
#'         }, starts, ends
#'     )
#' }
#' 
#' dat <- sentSplit(DATA, "state")
#' 
#' dat[["grams"]] <- sapply(dat[["state"]], function(x) { 
#'     unbag(n_gram(x, sep = "~~"))
#' })
#' 
#' m <- with(dat, as.tdm(grams, person))
#' rownames(m) <- gsub("~~", " ", rownames(m))
#' as.matrix(m)
#' rowSums(as.matrix(m))
#' 
#' 
#' 
#' dat2 <- sentSplit(raj, "dialogue")
#' 
#' dat2[["grams"]] <- sapply(dat2[["dialogue"]], function(x) { 
#'     unbag(n_gram(x, sep = "~~"))
#' })
#' 
#' m2 <- with(dat2, as.tdm(grams, person))
#' rownames(m2) <- gsub("~~", " ", rownames(m2))
#' qheat(t(as.matrix(tm:::weightTfIdf(tm::removeSparseTerms(m2, .7)))), high="red")
#' 
#' sort(rowSums(as.matrix(m2)))
#' }
ngrams <- function(text.var, grouping.var = NULL, n = 2, ...) {
    if(is.null(grouping.var)){
        grouping <- paste("row", 1:length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    out <- lapply(text.var, function(x) {
        if (is.na(x)) {
            return(NA)
        }
        ngram_helper(x, ns = n, ...)
    })

    names(out) <- grouping

    out2 <- split(out, names(out))

    oput2 <- lapply(out2, function(x) {
        names(x) <- paste(names(x), 1:length(names(x)))
        x
    })

    FUN <- function(x) unname(unlist(x))
    out3 <- lapply(out2, FUN)
    out4 <- lapply(out3, sort)
    out5 <- lapply(out4, function(x) {
        y <- strsplit(x, " ")
        lens <- sapply(y, length)
        z <- split(y, lens)
        names(z) <- paste0("n_", sort(unique(lens)))
        z
    })
    y <- strsplit(unname(sort(unlist(out3))), " ")
    lens <- sapply(y, length)
    out6 <- split(y, lens)
    names(out6) <- paste0("n_", sort(unique(lens)))
    o <- list(raw = out, group = out2, unlist1 = out3, unlist2 = out4, 
        group_n = out5, all = unname(sort(unlist(out3))), all_n = out6)
    class(o) <- "ngrams"
    o

}

#' Prints an ngrams object
#' 
#' Prints an ngrams object
#' 
#' @param x The ngrams object
#' @param \ldots ignored
#' @export
#' @method print ngrams
print.ngrams <-
function(x, ...) {
    print(x$all_n)
}


ngram_helper <- function(text, ns = 2, as.list = FALSE, ...) {
    x <- bag_o_words(text, ...)
    if (ns == 1) {
        return(x)
    }
    if (ns > length(x)) {
        ns <- length(x)
    }
    FUN2 <- function(NS, X =x, as.list = as.list) {
        inds <- data.frame(S=1:(length(x) - (NS -1)), 
            E = (NS):length(x))
        out <- lapply(1:nrow(inds), function(i) {
            paste(X[inds[i, 1]:inds[i, 2]], collapse=" ")
        })
        if (as.list) {
            out <- strsplit(out, " ")
        }
        out
    }
    c(x, unlist(lapply(2:ns, function(i) FUN2(i, x, as.list))))
}