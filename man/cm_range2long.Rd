\name{cm_range2long}
\alias{cm_range2long}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Codes to Start-End Durations
}
\description{
Transforms the range coding structure(s) from cm_range.temp (in list format) into a data frame of start and end durations in long format.
}
\usage{
cm_range2long(..., v.name = "variable", list.var = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{v.name}{
%%     ~~Describe \code{v.name} here~~
}
  \item{list.var}{
%%     ~~Describe \code{list.var} here~~
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
function (..., v.name = "variable", list.var = TRUE) 
{
    mf <- match.call(expand.dots = FALSE)
    objs <- as.character(mf[[2]])
    L1 <- lapply(objs, get)
    names(L1) <- objs
    L2 <- lapply(L1, cm_r2l, list.var = FALSE)
    if (list.var) {
        L2 <- lapply(seq_along(L2), function(i) data.frame(L2[[i]], 
            VAR = objs[i]))
    }
    DF <- data.frame(do.call(rbind, L2), row.names = NULL)
    if (list.var) {
        colnames(DF)[ncol(DF)] <- v.name
    }
    comment(DF) <- "cmrange"
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
