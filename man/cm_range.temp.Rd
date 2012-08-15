\name{cm_range.temp}
\alias{cm_range.temp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Template for Range Coding
}
\description{
Prints a template of range coding to the console.
}
\usage{
cm_range.temp(codes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{codes}{
%%     ~~Describe \code{codes} here~~
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
function (codes) 
{
    cat(paste("list(\n", paste0("    ", paste0("\b", paste(codes, 
        collapse = " = c(),\n    "), " = c()")), "\n)\n"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
