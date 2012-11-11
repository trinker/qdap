\name{cm_dummy2long}
\alias{cm_dummy2long}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cm_combine.dummy back to long
}
\description{
cm_combine.dummy back to long
}
\usage{
cm_dummy2long(cm.comb.obj, rm.var = "time")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cm.comb.obj}{
%%     ~~Describe \code{cm.comb.obj} here~~
}
  \item{rm.var}{
%%     ~~Describe \code{rm.var} here~~
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
function (cm.comb.obj, rm.var = "time") 
{
    L1 <- split(cm.comb.obj[, !colnames(cm.comb.obj) \%in\% rm.var], 
        cm.comb.obj[, rm.var])
    lng <- function(x) {
        a <- rle(x)
        lens <- a[[1]]
        ends <- cumsum(lens)[as.logical(a[[2]])]
        starts <- ends - lens[(as.logical(a[[2]]))]
        data.frame(start = starts, end = ends)
    }
    spanner <- function(A) {
        lapply(A, lng)
    }
    L2 <- lapply(L1, spanner)
    invisible(lapply(seq_along(L2), function(i) {
        tnms <- names(L2)[i]
        cnms <- names(L2[[i]])
        invisible(lapply(seq_along(L2[[i]]), function(j) {
            if (nrow(L2[[i]][[j]]) == 0) {
                return()
            }
            L2[[i]][[j]][, "code"] <<- cnms[j]
            L2[[i]][[j]][, rm.var] <<- names(L2)[i]
        }))
    }))
    L2 <- unlist(L2, recursive = FALSE)
    L2 <- L2[sapply(L2, function(x) nrow(x) != 0)]
    data.frame(do.call(rbind, L2), row.names = NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
