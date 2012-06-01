\name{freqTab2words}
\alias{freqTab2words}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a Word Frequency to Raw Words
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freqTab2words(word.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{word.list}{
%%     ~~Describe \code{word.list} here~~
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
function (word.list) 
{
    if (is.data.frame(word.list)) {
        rep(word.list[, 1], word.list[, 2])
    }
    else {
        lapply(word.list, function(x) rep(x[, 1], x[, 2]))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
