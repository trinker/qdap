\name{termco.c}
\alias{termco.c}
\alias{print.termco_c}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine Columns from a termco Object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termco.c(termco.d.object, combined.columns, new.name, zero.replace = 0, elim.old = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{termco.d.object}{
%%     ~~Describe \code{termco.d.object} here~~
}
  \item{combined.columns}{
%%     ~~Describe \code{combined.columns} here~~
}
  \item{new.name}{
%%     ~~Describe \code{new.name} here~~
}
  \item{zero.replace}{
%%     ~~Describe \code{zero.replace} here~~
}
  \item{elim.old}{
%%     ~~Describe \code{elim.old} here~~
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
function (termco.d.object, combined.columns, new.name, zero.replace = 0, 
    elim.old = TRUE) 
{
    x <- termco.d.object$raw
    y <- termco.d.object$prop
    if (!is.numeric(combined.columns)) {
        combined.columns <- which(names(x) \%in\%
        combined.columns)
    }
    x <- transform(x, new.name = rowSums(x[, combined.columns]), 
        check.names = FALSE)
    names(x)[length(x)] <- names(y)[length(y)] <- new.name
    y <- transform(y, new.name = rowSums(y[, combined.columns]), 
        check.names = FALSE)
    z <- if (elim.old) {
        seq_along(x)[!seq_along(x) \%in\% combined.columns]
    }
    else {
        seq_along(x)
    }
    x2 <- replacer(x, with = zero.replace)[, z]
    y2 <- replacer(y, with = zero.replace)[, z]
    DF <- replacer(termco.rnp(x, y), "0(0)", with = 
        zero.replace)[, z]
    o <- list(raw = x2, prop = y2, rnp = DF)
    class(o) <- "termco_c"
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
