\name{termco.rnp}
\alias{termco.rnp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Concatenation Merge of a termco and termco.p Objects
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termco.rnp(termco1, termco2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{termco1}{
%%     ~~Describe \code{termco1} here~~
}
  \item{termco2}{
%%     ~~Describe \code{termco2} here~~
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
function (termco1, termco2) 
{
    mypaste <- function(x, y) paste(x, "(", y, ")", sep = "")
    DF <- mapply(mypaste, termco1[, -c(1:2)], termco2[, -c(1:2)])
    DF <- data.frame(termco1[, 1:2], DF, check.names = FALSE)
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
