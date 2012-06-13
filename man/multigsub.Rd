\name{multigsub}
\alias{multigsub}
\alias{mgsub}
\title{
Takes a vector of search terms and a vector or single value of replacements
}
\description{
Takes a vector of search terms and a vector or single value of replacements
}
\usage{
multigsub(pattern, replacement, x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{replacement}{
%%     ~~Describe \code{replacement} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (pattern, replacement, x, ...) 
{
    key <- data.frame(pat = pattern, rep = replacement, stringsAsFactors = FALSE)
    msubs <- function(K, x, ...) {
        sapply(seq_len(nrow(K)), function(i) {
            x <<- gsub(K[i, 1], K[i, 2], x, ...)
        })
        return(x)
    }
    x <- msubs(K = key, x = x, ...)
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
