\name{stopwords}
\alias{stopwords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Removal of Specified Stopwords
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stopwords(textString, stopwords = Top25Words)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textString}{
%%     ~~Describe \code{textString} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
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
function (textString, stopwords = Top25Words) 
{
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    }
    else {
        stopwords
    }
    SW <- function(textString, stopwords) {
        "\%w/o\%" <- function(x, y) x[!x \%in\% y]
        unlist(strsplit(tolower(Trim(textString)), " ")) \%w/o\% 
            tolower(Trim(stopwords))
    }
    sapply(textString, function(x) SW(x, Stopwords))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
