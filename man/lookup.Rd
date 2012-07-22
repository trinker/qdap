\name{lookup}
\alias{lookup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An environment based lookup 
}
\description{
An fast environment based lookup (hash table)
}
\usage{
lookup(terms, key.match, key.reassign = NULL, missing = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{terms}{
%%     ~~Describe \code{terms} here~~
}
  \item{key.match}{
%%     ~~Describe \code{key.match} here~~
}
  \item{key.reassign}{
%%     ~~Describe \code{key.reassign} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
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
function (terms, key.match, key.reassign = NULL, missing = NA) 
{
    hash <- function(x, mode.out) {
        e <- new.env(hash = TRUE, size = nrow(x), parent = emptyenv())
        FUN <- paste0("as.", mode.out)
        FUN <- match.fun(FUN)
        apply(x, 1, function(col) assign(col[1], FUN(col[2]), 
            envir = e))
        return(e)
    }
    if (is.null(key.reassign)) {
        mode.out <- mode(key.match[, 2])
        DF <- key.match
        DF[, 1] <- as.character(DF[, 1])
    }
    else {
        mode.out <- mode(key.reassign)
        DF <- data.frame(as.character(key.match), key.reassign)
    }
    KEY <- hash(DF, mode.out = mode.out)
    recoder <- function(x, env, missing) {
        x <- as.character(x)
        rc <- function(x) {
            if (exists(x, env = env)) 
                get(x, e = env)
            else missing
        }
        sapply(x, rc, USE.NAMES = FALSE)
    }
    x <- recoder(terms, KEY, missing = missing)
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
