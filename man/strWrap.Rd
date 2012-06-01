\name{strWrap}
\alias{strWrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for strwrap that Writes to the Windows Clipboard
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
strWrap(text = "clipboard", width = 70)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
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
function (text = "clipboard", width = 70) 
{
    x <- if (text == "clipboard") {
        paste(readClipboard(), collapse = " ")
    }
    else {
        text
    }
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", x))
    x <- strwrap(x, width = width)
    writeClipboard(x, format = 1)
    writeLines(strwrap(x, width = width))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
