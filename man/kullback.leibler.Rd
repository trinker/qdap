\name{kullback.leibler}
\alias{kullback.leibler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kullback Leibler Statistic
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kullback.leibler(x, y = NULL, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y = NULL, digits = 3) 
{
    kl <- function(x, y) {
        x1 <- x/sum(x)
        y1 <- y/sum(y)
        x1[x1 == 0] <- NA
        y1[y1 == 0] <- NA
        z <- na.omit(data.frame(x1, y1))
        sum(z[, 1] * log(z[, 1]/z[, 2]))
    }
    if (is.null(y) & !is.null(comment(x))) {
        if (comment(x) \%in\% c("t.df")) {
            x <- x[, -c(1)]
        }
        else {
            if (is.null(y) & comment(x) \%in\% c("m.df")) {
                x <- x[-nrow(x), -c(1, ncol(x))]
            }
            else {
                x <- x
            }
        }
    }
    else {
        x <- x
    }
    if (is.null(y)) {
        z <- outer(colnames(x), colnames(x), Vectorize(function(i, 
            j) kl(x[, i], x[, j])))
        dimnames(z) <- list(colnames(x), colnames(x))
    }
    else {
        z <- kl(x = x, y = y)
    }
    z <- round(z, digits = digits)
    return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
