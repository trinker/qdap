\name{prop}
\alias{prop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a Matrix or Data Frame to Proportions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prop(mat, digits = 2, percent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{percent}{
%%     ~~Describe \code{percent} here~~
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
function (mat, digits = 2, percent = FALSE) 
{
    per <- if (percent) 
        100
    else 1
    daf <- data.frame(mat)
    daf <- sapply(seq_along(daf), function(x) round(per * (mat[, 
        x]/sum(mat)), digits = digits))
    mat2 <- as.matrix(daf)
    colnames(mat2) <- colnames(mat)
    return(mat2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
