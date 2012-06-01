\name{termco}
\alias{termco}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Count Occurances of Specified Terms
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termco(text.var, match.string, grouping.var = NULL, ignore.case = FALSE, zero.replace = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{match.string}{
%%     ~~Describe \code{match.string} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{ignore.case}{
%%     ~~Describe \code{ignore.case} here~~
}
  \item{zero.replace}{
%%     ~~Describe \code{zero.replace} here~~
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
function (text.var, match.string, grouping.var = NULL, ignore.case = FALSE, 
    zero.replace = 0) 
{
    group.var <- grouping.var
    x <- if (ignore.case) {
        lapply(text.var, function(x) term.count(tolower(x), mat = tolower(match.string)))
    }
    else {
        lapply(text.var, function(x) term.count(x, mat = match.string))
    }
    group.var <- if (is.list(group.var) & length(group.var) > 
        1) {
        apply(data.frame(group.var), 1, function(x) {
            if (any(is.na(x))) {
                NA
            }
            else {
                paste(x, collapse = ".")
            }
        })
    }
    else {
        grouping.var
    }
    y <- data.frame(text.var, do.call("rbind", x))
    if (is.null(group.var)) {
        names(y) <- c(as.character(substitute(text.var))[3], 
            paste("term(", match.string, ")", sep = ""))
        return(y)
    }
    else {
        y <- data.frame(group.var, y[, -1])
        X <- data.frame(Y = word.count(text.var), G = group.var)
        Z <- aggregate(Y ~ G, X, sum)
        z <- lapply(2:length(y), function(x) {
            aggregate(y[, x] ~ group.var, y, sum)
        })
        w <- data.frame(z[[1]][1], Z[, 2], lapply(seq_along(z), 
            function(x) z[[x]][, 2]))
        NAME <- if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
            paste(m, collapse = "&")
        }
        else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
        names(w) <- c(NAME, "word.count", paste("term(", match.string, 
            ")", sep = ""))
        w[, -c(1:2)] <- replacer(w[, -c(1:2)], replace = 0, with = zero.replace)
        return(w)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
