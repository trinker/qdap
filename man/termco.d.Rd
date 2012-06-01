\name{termco.d}
\alias{termco.d}
\alias{print.termco_d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convienence Wrapper to Combine termco, termco.p and termco.rnp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termco.d(text.var, match.string, grouping.var, ignore.case = FALSE, zero.replace = 0, output = "percent", digits = 2)
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
  \item{output}{
%%     ~~Describe \code{output} here~~
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
function (text.var, match.string, grouping.var, ignore.case = FALSE, 
    zero.replace = 0, output = "percent", digits = 2) 
{
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x)
            x[length(x)])
        paste(m, collapse = "&")
    }
    else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    x <- termco(text.var = text.var, match.string =
        match.string, 
        grouping.var = grouping.var)
    names(x)[1] <- NAME
    y <- termco.p(tco = x, output = output, digits = digits)
    if (zero.replace != 0) {
        x[, -c(1:2)] <- lapply(x[, -c(1:2)], function(x) 
            replacer(x, 0, zero.replace))
        y[, -c(1:2)] <- lapply(y[, -c(1:2)], function(x) 
            replacer(x, 0, zero.replace))
    }
    z <- termco.rnp(x, y)
    h <- paste(zero.replace, "(", zero.replace, ")", sep = "")
    z[, -c(1:2)] <- lapply(z[, -c(1:2)], function(x) replacer(x, 
        h, zero.replace))
    o <- list(raw = x, prop = y, rnp = z)
    class(o) <- "termco_d"
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
