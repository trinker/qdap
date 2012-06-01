\name{Search}
\alias{Search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search Terms for Colulmns of a Data Frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Search(term, dataframe, column.name, variation = 0.02, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
%%     ~~Describe \code{term} here~~
}
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{column.name}{
%%     ~~Describe \code{column.name} here~~
}
  \item{variation}{
%%     ~~Describe \code{variation} here~~
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
function (term, dataframe, column.name, variation = 0.02, ...) 
{
    te <- as.character(substitute(term))
    cn <- as.character(substitute(column.name))
    HUNT <- agrep(te, dataframe[, cn], ignore.case = TRUE, max.distance = variation, 
        ...)
    dataframe[c(HUNT), ]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
