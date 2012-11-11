\name{cm_code.overlap}
\alias{cm_code.overlap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Co-occurrence Between Codes
}
\description{
Combine co-occurances of codes into a new code.
}
\usage{
cm_overlap(x2long.obj, overlap.code.list, rm.var = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x2long.obj}{
%%     ~~Describe \code{x2long.obj} here~~
}
  \item{overlap.code.list}{
%%     ~~Describe \code{overlap.code.list} here~~
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
function (x2long.obj, overlap.code.list, rm.var = NULL) 
{
    NMS <- as.character(substitute(x2long.obj))
    if (!is.null(rm.var)) {
        if (!rm.var \%in\% colnames(x2long.obj)) {
            stop("rm.var does not match a column")
        }
    }
    x1 <- cm_long2dummy(x2long.obj, rm.var = rm.var)
    if (is.null(rm.var)) {
        colnames(x1) <- gsub("long.obj.", "", colnames(x1))
    }
    x2 <- cm_combine.dummy(x1, combine.code = overlap.code.list, 
        rm.var = rm.var)
    if (is.null(rm.var)) {
        x2$time <- NMS
    }
    rmv <- FALSE
    if (is.null(rm.var)) {
        rmv <- TRUE
        rm.var <- "time"
    }
    DF <- cm_dummy2long(x2, rm.var = rm.var)
    if (comment(x2long.obj) == "cmtime") {
        DF$Start <- convert(DF$start)
        DF$End <- convert(DF$end)
    }
    comment(x2long.obj)
    comment(DF) <- comment(x2long.obj)
    if (rmv) {
        DF$time <- NULL
    }
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
