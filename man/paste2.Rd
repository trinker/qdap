\name{paste2}
\alias{paste2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Paste an Unspecified Number Of Text Columns
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
paste2(multi.columns, sep = ".", handle.na = TRUE, trim = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{multi.columns}{
%%     ~~Describe \code{multi.columns} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{handle.na}{
%%     ~~Describe \code{handle.na} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
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
function (multi.columns, sep = ".", handle.na = TRUE, trim = TRUE) 
{
    if (trim) 
        multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        })
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call("cbind", multi.columns)
    }
    m <- if (handle.na) {
        apply(multi.columns, 1, function(x) {
            if (any(is.na(x))) {
                NA
            }
            else {
                paste(x, collapse = sep)
            }
        })
    }
    else {
        apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
