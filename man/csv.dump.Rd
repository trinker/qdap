\name{csv.dump}
\alias{csv.dump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a directory and place multiple csv files there
}
\description{
Create a directory and place multiple csv files there
}
\usage{
csv.dump(..., dir = NULL, open = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{open}{
%%     ~~Describe \code{open} here~~
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
function (..., dir = NULL, open = FALSE) 
{
    x <- match.call(expand.dots = FALSE)
    z <- as.character(x[[2]])
    x2 <- list(...)
    names(x2) <- z
    y <- qdap::folder(folder.name = dir)
    files <- paste0(y, "/", z, ".csv")
    lapply(seq_along(x2), function(i) {
        write.table(x2[i], file = files[i], sep = ",", col.names = T, 
            row.names = F, qmethod = "double")
    })
    if (open) {
        if (.Platform["OS.type"] == "windows") {
            shell.exec(y)
        }
        else {
            system(paste(Sys.getenv("R_BROWSER"), y))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
