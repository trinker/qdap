\name{word_diff_list}
\alias{word_diff_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Differences In Word Use Between Groups
}
\description{
Look at the differences in word uses between grouping variable(s).  Look at all possible "a" vs. "b" combinations or "a" vs. all others.
}
\usage{
word_diff_list(text.var, grouping.var, vs.all = FALSE, vs.all.cut = 1, stopwords = NULL, alphabetical = FALSE, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{vs.all}{
%%     ~~Describe \code{vs.all} here~~
}
  \item{vs.all.cut}{
%%     ~~Describe \code{vs.all.cut} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{alphabetical}{
%%     ~~Describe \code{alphabetical} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (text.var, grouping.var, vs.all = FALSE, vs.all.cut = 1, 
    stopwords = NULL, alphabetical = FALSE, digits = 2) 
{
    x <- wfm(text.var, grouping.var, stopwords = stopwords)
    x2 <- wfm(text.var, grouping.var, output = "prop", stopwords = stopwords, 
        digits = 3)
    if (!vs.all) {
        list_help <- function(i, j) {
            w <- x[, i]
            z <- x[, j]
            w2 <- x2[, i]
            z2 <- x2[, j]
            a <- data.frame(word = rownames(x)[w != 0 & z == 
                0], freq = w[w != 0 & z == 0], prop = w2[w != 
                0 & z == 0])
            b <- data.frame(word = rownames(x)[z != 0 & w == 
                0], freq = z[z != 0 & w == 0], prop = z2[z != 
                0 & w == 0])
            if (!alphabetical) {
                a <- a[order(-a$freq, a$word), ]
                b <- b[order(-b$freq, b$word), ]
            }
            rownames(a) <- rownames(b) <- NULL
            h <- list(a, b)
            names(h) <- paste0("unique_to_", colnames(x)[c(i, 
                j)])
            return(h)
        }
        m <- 1:ncol(x)
        n <- outer(m, m, paste, sep = ".")
        o <- do.call(rbind, lapply(strsplit(unlist(n[upper.tri(n)]), 
            "\\."), as.numeric))
        L1 <- lapply(1:nrow(o), function(k) {
            list_help(i = o[k, 1], j = o[k, 2])
        })
        names(L1) <- paste0(colnames(x)[o[, 1]], "_vs_", colnames(x)[o[, 
            2]])
    }
    else {
        x3 <- apply(x, 2, function(e) e > 0)
        g <- x3[rowSums(x3) < (vs.all.cut + 1), ]
        L1 <- lapply(1:ncol(g), function(i) {
            p <- rownames(x) \%in\% names(g[g[, i], i])
            z2 <- data.frame(x[p, i, drop = FALSE])
            z2$prop <- x2[p, i]
            colnames(z2)[1:2] <- c("freq", "prop")
            z2 <- data.frame(word = rownames(z2), z2)
            if (!alphabetical) {
                z2 <- z2[order(-z2$freq, z2$word), ]
            }
            rownames(z2) <- NULL
            return(z2)
        })
        if (vs.all.cut == 1) {
            names(L1) <- paste0("unique_to_", colnames(g))
        }
        else {
            names(L1) <- paste0("unique_to_", colnames(g), " | or_", 
                colnames(g), "_and_", (vs.all.cut - 1), "_other")
        }
    }
    return(L1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
