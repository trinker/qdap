\name{cm_combine.dummy}
\alias{cm_combine.dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Co-occurrence Between Codes
}
\description{
Combines code columns where they co-occur
}
\usage{
cm_combine.dummy(cm.l2d.obj, combine.code, rm.var = "time")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cm.l2d.obj}{
%%     ~~Describe \code{cm.l2d.obj} here~~
}
  \item{combine.code}{
%%     ~~Describe \code{combine.code} here~~
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
function (cm.l2d.obj, combine.code, rm.var = "time") 
{
    if (is.data.frame(cm.l2d.obj)) {
        NMS <- as.character(substitute(cm.l2d.obj))
        cm.l2d.obj <- list(cm.l2d.obj)
        names(cm.l2d.obj) <- NMS
    }
    invisible(lapply(seq_along(cm.l2d.obj), function(i) {
        cm.l2d.obj[[i]][, rm.var] <<- names(cm.l2d.obj)[i]
    }))
    DF <- data.frame(do.call(rbind, cm.l2d.obj), row.names = NULL)
    if (!is.list(combine.code)) {
        combine.code <- list(combine.code)
    }
    NEW <- lapply(seq_along(combine.code), function(i) {
        as.numeric(rowSums(DF[, c(combine.code[[i]])]) == length(combine.code[[i]]))
    })
    names(NEW) <- names(combine.code)
    tv <- ncol(DF)
    data.frame(DF[, -tv, drop = FALSE], do.call(cbind, NEW), 
        DF[, tv, drop = FALSE])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
