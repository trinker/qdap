\name{dissimilarity}
\alias{dissimilarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dissimilarity Statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dissimilarity(wfm.object, method = "euclidean", diag = FALSE, upper = FALSE, p = 2, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wfm.object}{
%%     ~~Describe \code{wfm.object} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{diag}{
%%     ~~Describe \code{diag} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
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
function (wfm.object, method = "euclidean", diag = FALSE, upper = FALSE, 
    p = 2, digits = 3) 
{
    if (comment(wfm.object) != "true.matrix") {
        warning("not a matrix from word.freq.matrix function")
    }
    if (comment(wfm.object) != "true.matrix") {
        wfm.object <- wfm.object[-c(nrow(wfm.object)), -c(1, 
            ncol(wfm.object))]
        wfm.object <- t(wfm.object)
    }
    else {
        wfm.object <- t(wfm.object)
    }
    x <- stats::dist(wfm.object, method = method, diag = diag, 
        upper = upper, p = p)
    return(round(x, digits = digits))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
