\name{strip}
\alias{strip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Conversion of Text to Lower Case and Words Only
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
strip(x, digit.remove = TRUE, apostrophe.remove = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digit.remove}{
%%     ~~Describe \code{digit.remove} here~~
}
  \item{apostrophe.remove}{
%%     ~~Describe \code{apostrophe.remove} here~~
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
function (x, digit.remove = TRUE, apostrophe.remove = FALSE) 
{
    strp <- function(x, digit.remove, apostrophe.remove) {
        x2 <- Trim(tolower(gsub(".*?($|'|[^[:punct:]]).*?", "\\1", 
            as.character(x))))
        x2 <- if (apostrophe.remove) 
            gsub("'", "", x2)
        else x2
        ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", 
            x2), x2)
    }
    unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
