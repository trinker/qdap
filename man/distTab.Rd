\name{distTab}
\alias{distTab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate SPSS Style Frequency Tables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
distTab(dataframe, cuts = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{cuts}{
%%     ~~Describe \code{cuts} here~~
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
function (dataframe, cuts = NULL) 
{
    DF <- as.data.frame(dataframe)
    freq.dist <- function(var, breaks = cuts) {
        x1 <- substitute(var)
        VAR <- if (is.null(cuts)) {
            var
        }
        else {
            if (is.numeric(var) & length(table(var)) > cuts) {
                cut(var, breaks, labels = NULL, include.lowest = FALSE, 
                  right = TRUE, dig.lab = 3, ordered_result = FALSE)
            }
            else {
                var
            }
        }
        x <- data.frame(table(VAR))
        names(x)[1] <- as.character(x1)
        percent <- x[, 2]/sum(x[, 2]) * 100
        data.frame(interval = x[, 1], Freq = x[, 2], cum.Freq = cumsum(x[, 
            2]), percent, cum.percent = cumsum(percent))
    }
    suppressWarnings(lapply(DF, FUN = freq.dist))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
