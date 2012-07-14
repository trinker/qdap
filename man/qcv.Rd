\name{qcv}
\alias{qcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quick character vector creation
}
\description{
Quick character vector creation
}
\usage{
qcv(terms, sep = ",", fixed = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{terms}{
%%     ~~Describe \code{terms} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
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
function (terms, sep = ",", fixed = TRUE, ...) 
{
    x <- as.character(substitute(terms))
    z <- Trim(unlist(strsplit(x, split = sep, fixed = fixed, 
        ...)))
    return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
