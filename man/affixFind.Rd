\name{affixFind}
\alias{affixFind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Affixes
}
\description{
Searches a list of words and returns those with a specific affix
}
\usage{
affixFind(affix.string, char.string, affix = "suffix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affix.string}{
%%     ~~Describe \code{affix.string} here~~
}
  \item{char.string}{
%%     ~~Describe \code{char.string} here~~
}
  \item{affix}{
%%     ~~Describe \code{affix} here~~
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
function (affix.string, char.string, affix = "suffix") 
{
    n.char <- nchar(affix.string)
    AFF <- affix(char.string = char.string, n.char = n.char, 
        affix = affix)
    AFF[which(AFF == affix.string)]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
