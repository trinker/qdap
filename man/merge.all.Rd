\name{merge.all}
\alias{merge.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge Multiple Data Sets
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
merge.all(frames, by, na.replace = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
%%     ~~Describe \code{frames} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{na.replace}{
%%     ~~Describe \code{na.replace} here~~
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
function (frames, by, na.replace = NA) 
{
    DF <- Reduce(function(x, y) {
        merge(x, y, by = by, all = TRUE)
    }, frames)
    return(NAer(DF, replace = na.replace))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
