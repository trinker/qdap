\name{qview}
\alias{qview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for Head that Displays Additional Data Frame Information
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qview(dataframe, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (dataframe, ...) 
{
    x <- as.character(substitute(dataframe))
    cat(paste(rep("=", 72), collapse = ""), "\n", "n = ", nrow(dataframe), 
        "          # of vars = ", ncol(dataframe), "           ", 
        x, "\n", "\b", paste(rep("=", 72), collapse = ""), "\n")
    return(head(dataframe, ...))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
