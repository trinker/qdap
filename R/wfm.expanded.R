#' Expanded Word Frequency Matrix
#' 
#' Expand a word frequency matrix to have multiple rows for each word.
#' 
#' @param rm.var %% ~~Describe \code{rm.var} here~~
#' @param text.var The text variable or an a word frequency matrix object
#' @param grouping.var The grouping variables. Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @return Returns a matrix similar to a word frequency matrix but the rows are expanded to represent the maximum usages of the word and cells are dummy coded to indicate that numer of uses.
#' @seealso \code{\link[qdap]{word.freq.matrix}}, 
#' \code{\link[qdap]{wfm}}
#' @examples
#' z <- wfm(DATA$state, DATA$person)
#' wfm.expanded(z)
#' wfm.expanded(DATA$state, DATA$person)
#' wfm.expanded(DATA$state, list(DATA$sex, DATA$adult))
#' wfm.expanded(CO2) #error
wfm.expanded <-
function(text.var, grouping.var = NULL){
    if(is.null(comment(text.var))) {
        z <- wfm(text.var, grouping.var)
    } else {
        if (comment(text.var)== "true.matrix") {
            z <- text.var
        } else {
            stop("Must supply a text variable or a word frequency matrix")
        }
    }
    rows <-lapply(1:nrow(z), function(i) z[i, ])
    names(rows) <- rownames(z)
    lens <- sapply(1:nrow(z), function(i) max(z[i, ]))
    rep(rownames(z), lens)
    repper <- function(R) {
        mx <- max(R)
        sapply(R, function(x) c(rep(1, x), rep(0, mx-x)))
    }
    expanded <- do.call(rbind, lapply(1:nrow(z), function(i) repper(z[i, ])))
    rownames(expanded) <- rep(rownames(z), lens)
    expanded
}
