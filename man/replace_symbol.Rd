\name{replace_symbol}
\alias{replace_symbol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Change Symbols for Word Equivalents
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
symbol_change(text.var, dollar = TRUE, percent = TRUE, pound = TRUE, at = TRUE, and = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{dollar}{
%%     ~~Describe \code{dollar} here~~
}
  \item{percent}{
%%     ~~Describe \code{percent} here~~
}
  \item{pound}{
%%     ~~Describe \code{pound} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
}
  \item{and}{
%%     ~~Describe \code{and} here~~
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
function (text.var, dollar = TRUE, percent = TRUE, pound = TRUE, 
    at = TRUE, and = TRUE) 
{
    sch <- function(text, dollar = TRUE, percent = TRUE, pound = TRUE, 
        at = TRUE, and = TRUE) {
        x <- ifelse(percent == TRUE, gsub("\%", " percent ", text), 
            text)
        x <- ifelse(dollar == TRUE, gsub("$", " dollars ", x, 
            fixed = TRUE), x)
        x <- ifelse(pound == TRUE, gsub("#", " number ", x, fixed = TRUE), 
            x)
        x <- ifelse(and == TRUE, gsub("&", " and ", x, fixed = TRUE), 
            x)
        x <- ifelse(at == TRUE, gsub("@", " at ", x, fixed = TRUE), 
            x)
        gsub(" +", " ", x)
    }
    unlist(lapply(text.var, function(text) sch(text, dollar = dollar, 
        percent = percent, pound = pound, at = at, and = and)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
