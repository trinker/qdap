\name{colsplit2df}
\alias{colsplit2df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for colSplit that Returns a Data Frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colsplit2df(dataframe, column = 1, orig.keep = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{column}{
%%     ~~Describe \code{column} here~~
}
  \item{orig.keep}{
%%     ~~Describe \code{orig.keep} here~~
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
function (dataframe, column = 1, orig.keep = FALSE, ...) 
{
    nc <- if (!is.numeric(column)) {
        match(column, names(dataframe))
    }
    else {
        column
    }
    ncols <- colSplit(dataframe[, column, drop = FALSE], ...)
    if (orig.keep) {
        cbind(dataframe[, nc, drop = FALSE], ncols, dataframe[, 
            -nc])
    }
    else {
        cbind(ncols, dataframe[, -nc])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
