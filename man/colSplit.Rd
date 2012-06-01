\name{colSplit}
\alias{colSplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separate a Column Pasted by paste2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colSplit(column, name.sep = "&", col.sep = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column}{
%%     ~~Describe \code{column} here~~
}
  \item{name.sep}{
%%     ~~Describe \code{name.sep} here~~
}
  \item{col.sep}{
%%     ~~Describe \code{col.sep} here~~
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
function (column, name.sep = "&", col.sep = ".") 
{
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call("rbind", svar))
    if (length(unlist(strsplit(names(column), name.sep, fixed = TRUE))) > 
        1) {
        cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
        names(svar) <- cn
    }
    return(svar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
