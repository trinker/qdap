\name{replacer}
\alias{replacer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replace Cells in a Matrix or DataFrame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
replacer(dat, replace = 0, with = "-")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{replace}{
%%     ~~Describe \code{replace} here~~
}
  \item{with}{
%%     ~~Describe \code{with} here~~
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
function (dat, replace = 0, with = "-") 
{
    h <- is.vector(dat)
    i <- is.matrix(dat)
    j <- is.data.frame(dat)
    if (is.numeric(replace)) {
        NAMES <- names(dat)
        not.num <- Negate(is.numeric)
        dat1 <- dat[, sapply(dat, not.num)]
        dat2 <- dat[, sapply(dat, is.numeric)]
    }
    else {
        dat2 <- dat
    }
    y <- as.matrix(dat2)
    if (is.na(replace)) {
        y[is.na(y)] <- with
    }
    else {
        y[y == replace] <- with
    }
    if (is.numeric(replace)) {
        y <- data.frame(dat1, y, check.names = FALSE)
        m <- NAMES[!NAMES \%in\% names(y)]
        if (length(m) == 1) 
            names(y)[1] <- m
        y <- y[, NAMES]
    }
    if (h) 
        y <- as.vector(y)
    if (i) 
        y <- as.matrix(y)
    if (j) 
        y <- as.data.frame(y, check.names = FALSE)
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
