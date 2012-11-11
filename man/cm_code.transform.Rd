\name{cm_code.transform}
\alias{cm_code.transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Codes
}
\description{
Transform co-occurences and/or combinations of codes into a new code(s).
}
\usage{
cm_code.transform(x2long.obj, overlap.code.list = NULL, combine.code.list = NULL, rm.var = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x2long.obj}{
%%     ~~Describe \code{x2long.obj} here~~
}
  \item{overlap.code.list}{
%%     ~~Describe \code{overlap.code.list} here~~
}
  \item{combine.code.list}{
%%     ~~Describe \code{combine.code.list} here~~
}
  \item{rm.var}{
%%     ~~Describe \code{rm.var} here~~
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
function (x2long.obj, overlap.code.list = NULL, combine.code.list = NULL, 
    rm.var = NULL) 
{
    A <- B <- NULL
    if (!is.null(overlap.code.list)) {
        A <- cm_code.overlap(x2long.obj = x2long.obj, overlap.code.list = overlap.code.list, 
            rm.var = rm.var)
    }
    if (!is.null(combine.code.list)) {
        B <- cm_code.combine(x2long.obj = x2long.obj, combine.code.list = combine.code.list, 
            rm.var = rm.var)
    }
    DF <- data.frame(rbind(A, B))
    DF <- DF[!duplicated(DF), ]
    rownames(DF) <- NULL
    if (!is.null(rm.var)) {
        DF <- DF[order(DF[, rm.var]), ]
    }
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
