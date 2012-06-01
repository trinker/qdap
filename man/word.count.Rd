\name{word.count}
\alias{word.count}
\alias{wc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Word Counts
}
\description{
Transcript Apply Word Counts
}
\usage{
word.count(text, by = "row", missing = NA, digit.remove = TRUE)
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
  \item{digit.remove}{
%%     ~~Describe \code{digit.remove} here~~
}
  \item{names}{
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
function(text, by = "row", missing = NA, 
    digit.remove = TRUE, names = TRUE) {
    txt <- sapply(as.character(text), function(x) {
            ifelse(is.na(x)|is.null(x), "", x)
        }
    )
    OP <- switch(by, 
         all = {length(unblanker(unlist(word.split(
                   reducer(unlist(strip(
                   paste(as.character(txt), collapse = " "), 
                   digit.remove = digit.remove)))))))}, 
         row = {sapply(txt, function(x) length(unblanker(
                   unlist(word.split(
                   reducer(unlist(strip(as.character(x),
                   digit.remove = 
                   digit.remove))))))))}
    )
    z <- ifelse(OP==0, missing, OP)
    if(!names) names(z) <- NULL
    return(z)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
