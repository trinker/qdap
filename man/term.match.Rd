\name{term.match}
\alias{term.match}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a list of words that matching functions will match to
}
\description{
Generate a list of words that matching functions will match to
}
\usage{
termco.mt(terms, text.var, return.list = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{terms}{
%%     ~~Describe \code{terms} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{return.list}{
%%     ~~Describe \code{return.list} here~~
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
function (terms, text.var, return.list = TRUE) 
{
    y <- stopwords(text.var, stopwords = NULL, unlist = TRUE, 
        strip = TRUE, unique = TRUE)
    x <- lapply(unlist(terms), function(z) {
        v <- term.find(y, mat = z, logic = TRUE)
        y[v]
    })
    names(x) <- unlist(terms)
    if (!return.list) {
        x <- sort(unique(unlist(x)))
    }
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
