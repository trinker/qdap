\name{termco.p}
\alias{termco.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform a termco Object to Proportions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termco.p(tco, output = "percent", digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tco}{
%%     ~~Describe \code{tco} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (tco, output = "percent", digits = 2) 
{
    a <- tco[, -c(1:2)]
    b <- tco[, 2]
    e <- tco[, 1:2]
    d <- switch(output, percent = lapply(a, function(x) round(100 * 
        (x/b), digits = digits)), proportion = lapply(a, function(x) round(x/b, 
        digits = digits)), per = lapply(a, function(x) round(100 * 
        (x/b), digits = digits)), prop = lapply(a, function(x) round(x/b, 
        digits = digits)))
    return(data.frame(e, d, check.names = FALSE))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
