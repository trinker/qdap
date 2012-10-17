\name{qprep}
\alias{qprep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quick Preparation of Text
}
\description{
Wrapper for bracketX, replace_number, replace_symbol, replace_abbreviation and scrubber to quickly prepares text for analysis.  Care should taken with this function to ensure data is properly formatted and complete.
}
\usage{
qprep(text.var, bracket = "all", missing = NULL, names = FALSE, abbreviation = qdap::abbreviations, replace = NULL, ignore.case = TRUE, num.paste = "separate")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{bracket}{
%%     ~~Describe \code{bracket} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
}
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{abbreviation}{
%%     ~~Describe \code{abbreviation} here~~
}
  \item{replace}{
%%     ~~Describe \code{replace} here~~
}
  \item{ignore.case}{
%%     ~~Describe \code{ignore.case} here~~
}
  \item{num.paste}{
%%     ~~Describe \code{num.paste} here~~
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
function (text.var, bracket = "all", missing = NULL, names = FALSE, 
    abbreviation = qdap::abbreviations, replace = NULL, ignore.case = TRUE, 
    num.paste = "separate") 
{
    x <- bracketX(text.var, bracket = bracket, missing = missing, 
        names = names)
    x <- replace_abbreviation(x, abbreviation = abbreviation, 
        replace = replace, ignore.case = ignore.case)
    x <- replace_number(x, num.paste = num.paste)
    Trim(scrubber(replace_symbol(x)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
