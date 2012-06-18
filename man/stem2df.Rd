\name{stem2df}
\alias{stem2df}
\title{
Stems a vector of text strings and outputs a data frame
}
\description{
Input a data frame with a text variable and outputs the data frame with the stemmed text variable appended
}
\usage{
stem2df(dataframe, text.var, stem.name = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{stem.name}{
%%     ~~Describe \code{stem.name} here~~
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
function (dataframe, text.var, stem.name = NULL, ...) 
{
    if (!is.data.frame(dataframe)) {
        stop("Please supply a data.frame to stem2df")
    }
    if (is.numeric(dataframe[, text.var])) {
        stop("text.var can not be numeric")
    }
    new <- stemmer(dataframe[, text.var])
    DF <- data.frame(dataframe, stem.text = new, stringsAsFactors = FALSE)
    if (!is.null(stem.name)) {
        names(DF)[ncol(DF)] <- stem.name
    }
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
