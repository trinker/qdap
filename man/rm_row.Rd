\name{rm_row}
\alias{rm_row}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Rows Based On Supplied Terms
}
\description{
Remove rows from a dataframe based on supplied terms/partial terms (must be an exact match)
}
\usage{
rm_row(dataframe, search.column, terms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{search.column}{
%%     ~~Describe \code{search.column} here~~
}
  \item{terms}{
%%     ~~Describe \code{terms} here~~
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
function (dataframe, search.column, terms) 
{
    FUN <- function(dat, sc, term) {
        dat[substring(dat[, sc], 1, nchar(term)) != term, ]
    }
    DF <- dataframe
    lapply(terms, function(x) {
        DF <<- FUN(DF, search.column, x)
    })
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
