\name{word.freq.matrix}
\alias{word.freq.matrix}
\alias{wfm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Word Ffrequency Matrix by Person
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
word.freq.matrix(wfdf = NULL, text.var = NULL, grouping.var = NULL, stopwords = NULL, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wfdf}{
%%     ~~Describe \code{wfdf} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (wfdf = NULL, text.var = NULL, grouping.var = NULL, 
    stopwords = NULL, digits = 2) 
{
    if (!is.null(wfdf)) {
        if (comment(wfdf) == "t.df") {
            wfdf <- wfdf
        }
        else {
            if (comment(wfdf) == "m.df") {
                wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
            }
            else {
                stop("Object must be a raw word frequency data frame")
            }
        }
        x2 <- wfdf[, -1]
        rownames(x2) <- wfdf[, 1]
        x2 <- as.matrix(x2)
    }
    else {
        if (!is.null(text.var) & !is.null(grouping.var)) {
            wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
                stopwords = stopwords)
            x2 <- wfdf[, -1]
            rownames(x2) <- wfdf[, 1]
            x2 <- as.matrix(x2)
        }
        else {
            stop("must specify both text.var & grouping var or wfdf")
        }
    }
    comment(x2) <- "true.matrix"
    return(x2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
