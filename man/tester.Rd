\name{tester}
\alias{tester}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search a Vector of Vectors for Condition (Code)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tester(string, code, type = "logical")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
%%     ~~Describe \code{string} here~~
}
  \item{code}{
%%     ~~Describe \code{code} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (string, code, type = "logical") 
{
    C <- as.character(substitute(code))
    test <- function(S, x) any(S == x)
    x <- sapply(string, function(x) test(x, C))
    if (type == "logical") {
        x
    }
    else {
        if (type == "numeric") {
            which(x)
        }
        else {
            stop("type must be logical or numeric")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
