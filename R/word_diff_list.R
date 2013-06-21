#' Differences In Word Use Between Groups
#' 
#' Look at the differences in word uses between grouping variable(s).  Look at 
#' all possible "a" vs. "b" combinations or "a" vs. all others.
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param vs.all logical. If \code{TRUE} looks at each grouping variable against 
#' all others ("a" vs. all comparison).  If \code{FALSE} looks at each "a" vs. 
#' "b", comparison (e.g., for groups "a", "b", and "c"; "a" vs. "b", "a" vs. "c" 
#' and "b" vs. "c" will be considered).
#' @param vs.all.cut Controls the number of other groups that may share a word 
#' (default is 1).
#' @param stopwords A vector of stop words to remove.
#' @param alphabetical logical. If \code{TRUE} orders the word lists 
#' alphabetized by word.  If \code{FALSE} order first by frequency and then by 
#' word.
#' @param digits the number of digits to be displayed in the proportion column 
#' (default is 3).
#' @return An list of word data frames comparing grouping variables word use 
#' against one another. Each dataframe contains three columns:
#' \item{word}{The words unique to that group} 
#' \item{freq}{The number of times that group used that word}
#' \item{prop}{The proportion of that group's overall word use dedicated to that 
#' particular word}
#' @keywords word-list
#' @export
#' @examples
#' \dontrun{
#' out1 <- with(DATA, word_diff_list(text.var = state, 
#'     grouping.var = list(sex, adult)))
#' lapply(unlist(out1, recursive = FALSE), head, n=3)
#' 
#' out2 <- with(DATA, word_diff_list(state, person))
#' lapply(unlist(out2, recursive = FALSE), head, n=3)
#' 
#' out3 <- with(DATA, word_diff_list(state, grouping.var = list(sex, adult), 
#'     vs.all=TRUE, vs.all.cut=2))
#' 
#' 
#' out4 <- with(mraja1, word_diff_list(text.var = dialogue, 
#'     grouping.var = list(mraja1$sex, mraja1$fam.aff)))
#' 
#' 
#' out5 <- word_diff_list(mraja1$dialogue, mraja1$person)
#' 
#' out6 <- word_diff_list(mraja1$dialogue, mraja1$fam.aff, stopwords = Top25Words)
#' 
#' out7 <- word_diff_list(mraja1$dialogue, mraja1$fam.aff, vs.all=TRUE, vs.all.cut=2)
#' lapply(out7, head, n=3)
#' }
word_diff_list <-
function(text.var, grouping.var, vs.all = FALSE, 
    vs.all.cut = 1, stopwords = NULL, alphabetical = FALSE, digits = 2){
    x <- wfm(text.var, grouping.var, stopwords=stopwords)
    x2 <- wfm(text.var, grouping.var,  output = "prop", 
       stopwords=stopwords)
    if (!vs.all) {
        list_help <- function(i, j) {
            w <- x[, i]
            z <- x[, j]
            w2 <- x2[, i]
            z2 <- x2[, j]
            a <- data.frame(word=rownames(x)[w!=0 & z==0], freq=w[w!=0 & z==0], 
                prop=w2[w!=0 & z==0])
            b <- data.frame(word=rownames(x)[z!=0 & w==0], freq=z[z!=0 &w==0], 
                prop=z2[z!=0 &w==0])
            if (!alphabetical) {
                a <- a[order(-a$freq, a$word), ]
                b <- b[order(-b$freq, b$word), ]
            }
            rownames(a) <- rownames(b) <- NULL
            h <- list(a, b)
            names(h) <- paste0("unique_to_", colnames(x)[c(i, j)])
            return(h)
        }
        m <- 1:ncol(x)
        n <- outer(m , m, paste, sep=".")
        o <- do.call(rbind, lapply(strsplit(unlist(n[upper.tri(n)]), "\\."), 
            as.numeric))
        L1 <- lapply(1:nrow(o), function(k) {
            list_help(i = o[k, 1], j=o[k, 2])
            }
        )
        names(L1) <- paste0(colnames(x)[o[, 1]], "_vs_", colnames(x)[o[, 2]])
    } else {
        x3 <- apply(x, 2, function(e) e > 0)
        g <- x3[rowSums(x3)< (vs.all.cut + 1), ]
        L1 <- lapply(1:ncol(g), function(i) {
                p <- rownames(x) %in% names(g[g[, i], i])
                z2 <- data.frame(x[p, i, drop=FALSE])
                z2$prop <- x2[p, i]
                colnames(z2)[1:2] <- c("freq", "prop")
                z2 <- data.frame(word = rownames(z2), z2)
                if (!alphabetical) {
                    z2 <- z2[order(-z2$freq, z2$word), ]
                }
                rownames(z2) <- NULL
                return(z2)
            }
        )
        if(vs.all.cut == 1) {
            names(L1) <- paste0("unique_to_", colnames(g))
        } else {
            names(L1) <- paste0("unique_to_", colnames(g), 
                " | or_", colnames(g), "_and_", (vs.all.cut-1), "_other")
        }
    }
    return(L1)
}
