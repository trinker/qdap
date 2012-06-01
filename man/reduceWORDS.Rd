\name{reduceWORDS}
\alias{reduceWORDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Takes a Word Frequency List and Coverts to Raw Words
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reduceWORDS(rfwlDF_LIST)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rfwlDF_LIST}{
%%     ~~Describe \code{rfwlDF_LIST} here~~
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
function (rfwlDF_LIST) 
{
    RFL <- function(rfwlDF) {
        BB <- rfwlDF
        unlist(as.character(BB[rep(seq(dim(BB)[1]), BB$FREQ), 
            1]))
    }
    lapply(rfwlDF_LIST, RFL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
