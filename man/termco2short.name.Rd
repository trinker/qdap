\name{termco2short.name}
\alias{termco2short.name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a termco object to a termco object with shortened column names.
}
\description{
Eliminates the ``term()'' wrap from the column names of a termco object.
}
\usage{
termco.short.name(termco.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{termco.object}{
%%     ~~Describe \code{termco.object} here~~
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
function (termco.object) 
{
    CLASS <- class(termco.object)
    short.name <- function(df) {
        mn <- gsub("(.*)\\)([^\\)]*)", "\\1\\2", colnames(df))
        colnames(df) <- gsub("term(", "", mn, fixed = TRUE)
        return(df)
    }
    o <- lapply(termco.object, function(x) {
        if (is.data.frame(x)) {
            short.name(x)
        }
        else {
            x
        }
    })
    class(o) <- CLASS
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
