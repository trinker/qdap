\name{character.count}
\alias{character.count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Count Number of Characters Used
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
character.count(text, by = "row", missing = NA, apostrophe = TRUE, digit.remove = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
}
  \item{apostrophe}{
%%     ~~Describe \code{apostrophe} here~~
}
  \item{digit.remove}{
%%     ~~Describe \code{digit.remove} here~~
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
function (text, by = "row", missing = NA, apostrophe = TRUE, 
    digit.remove = TRUE) 
{
    text2 <- if (apostrophe == TRUE) {
        text
    }
    else {
        gsub("'", "", text, fixed = TRUE)
    }
    chara <- function(x) {
        y <- unlist(strsplit(strip(x, digit.remove = digit.remove), 
            NULL))
        z <- subset(y, y != " ")
        length(z)
    }
    OP <- switch(by, all = chara(paste(unlist(text2), collapse = "  ")), 
        row = unlist(lapply(text2, function(x) chara(x))))
    ifelse(OP == 0, missing, OP)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
