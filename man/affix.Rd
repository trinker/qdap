\name{affix}
\alias{affix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Affix Locator
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
affix(char.string, n.char, affix = "suffix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{char.string}{
%%     ~~Describe \code{char.string} here~~
}
  \item{n.char}{
%%     ~~Describe \code{n.char} here~~
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
function (char.string, n.char, affix = "suffix") 
{
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    ender <- function(x, s) {
        nc <- nchar(x)
        substring(x, (nc - s + 1), nc)
    }
    beginer <- function(x, s) substring(x, 1, s)
    if (affix == "suffix") {
        sapply(Trim(as.character(char.string)), function(x) ender(x, 
            n.char))
    }
    else {
        if (affix == "prefix") {
            sapply(Trim(as.character(char.string)), function(x) beginer(x, 
                n.char))
        }
        else {
            cat("\nWARNING:\nIncorrect affix argument!\n\n")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
