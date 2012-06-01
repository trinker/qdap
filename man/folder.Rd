\name{folder}
\alias{folder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a Folder Working Directory
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
folder(folder.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder.name}{
%%     ~~Describe \code{folder.name} here~~
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
function (folder.name = NULL) 
{
    FN <- if (is.null(folder.name)) {
        SS <- gsub(":", ".", substr(Sys.time(), 1, 19))
        paste(substr(SS, 1, 10), "  Time", substr(SS, 11, 19), 
            sep = "")
    }
    else {
        as.character(substitute(folder.name))
    }
    x <- paste(getwd(), "/", FN, sep = "")
    dir.create(x)
    return(FN)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
