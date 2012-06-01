\name{blank2NA}
\alias{blank2NA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add NA Values to Blank Cells 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
blank2NA(dataframe, missing = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
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
function (dataframe, missing = NA) 
{
    cn <- which(sapply(dataframe, is.character))
    FUN <- function(da) {
        if (is.character(da)) 
            da <- Trim(da)
        da[da == ""] <- missing
        names(da) <- NULL
        return(da)
    }
    DF <- data.frame(apply(dataframe, 2, FUN), check.names = FALSE)
    sapply(cn, function(i) {
        DF[, i] <<- as.character(DF[, i])
    })
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
