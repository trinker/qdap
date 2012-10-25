\name{wfm.expanded}
\alias{wfm.expanded}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expanded Word Frequency Matrix
}
\description{
Expand a word frequency matrix to have multiple rows for each word.  
}
\usage{
wfm.expanded(text.var, grouping.var = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
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
function (text.var, grouping.var = NULL) 
{
    if (is.null(comment(text.var))) {
        z <- wfm(text.var, grouping.var)
    }
    else {
        if (comment(text.var) == "true.matrix") {
            z <- text.var
        }
        else {
            stop("Must supply a text variable or a word frequency matrix")
        }
    }
    rows <- lapply(1:nrow(z), function(i) z[i, ])
    names(rows) <- rownames(z)
    lens <- sapply(1:nrow(z), function(i) max(z[i, ]))
    rep(rownames(z), lens)
    repper <- function(R) {
        mx <- max(R)
        sapply(R, function(x) c(rep(1, x), rep(0, mx - x)))
    }
    expanded <- do.call(rbind, lapply(1:nrow(z), function(i) repper(z[i, 
        ])))
    rownames(expanded) <- rep(rownames(z), lens)
    expanded
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
