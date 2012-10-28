\name{mcsv_r}
\alias{mcsv_r}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Multiple csv Files at a Time
}
\description{
Read and assign multiple csv files at the same time.
}
\usage{
mcsv_r(files, a.names = NULL, l.name = NULL, list = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{a.names}{
%%     ~~Describe \code{a.names} here~~
}
  \item{l.name}{
%%     ~~Describe \code{l.name} here~~
}
  \item{list}{
%%     ~~Describe \code{list} here~~
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
function (files, a.names = NULL, l.name = NULL, list = TRUE) 
{
    if (is.null(a.names)) {
        a.names <- sapply(files, function(x) {
            v <- unlist(strsplit(x, "/|\\\\"))
            gsub(".csv", "", v[length(v)])
        })
    }
    invisible(lapply(seq_along(files), function(i) {
        assign(a.names[i], read.csv(files[i]), envir = .GlobalEnv)
    }))
    if (list) {
        L1 <- lapply(a.names, function(x) {
            get(a.names)
        })
        if (is.null(l.name)) {
            l.name <- "L1"
        }
        assign(l.name, L1, envir = .GlobalEnv)
    }
    assi <- paste(c(l.name, a.names), collapse = ", ")
    message(paste0("objects assigned: ", assi))
    invisible(a.names)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
