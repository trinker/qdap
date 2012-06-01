\name{outlier.labeler}
\alias{outlier.labeler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Categorize and Code Outliers in Numeric String
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
outlier.labeler(x, standardize=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{x}{
%%     ~~Describe \code{standardize} here~~
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
function(x, standardize=TRUE){
    if (standardize) x <- scale(x)
    y <- ifelse(abs(x) >= 3, "3sd", 
        ifelse(abs(x) >= 2 & abs(x) < 3, "2sd", 
        ifelse(abs(x) >= 1.5 & abs(x) < 2, 
        "1.5sd", "-")))
    return(y)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
