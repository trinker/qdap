\name{cm_combine}
\alias{cm_combine}
\title{
Combine Multiple Codes Columns
}
\description{
Combine multiple codes columns by parent nodes.
}
\usage{
cm_combine(dataframe, combined.columns, elim.old = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{combined.columns}{
%%     ~~Describe \code{combined.columns} here~~
}
  \item{elim.old}{
%%     ~~Describe \code{elim.old} here~~
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
function (dataframe, combined.columns, elim.old = TRUE) 
{
    combo <- function(dat, comb, new.name, elim.old) {
        DF <- data.frame(dat, as.numeric(rowSums(dat[, comb]) > 
            0))
        names(DF)[ncol(DF)] <- new.name
        if (elim.old) {
            DF <- DF[, !names(DF) \%in\% comb]
        }
        return(DF)
    }
    lapply(seq_along(combined.columns), function(i) {
        dataframe <<- combo(dat = dataframe, comb = combined.columns[[i]], 
            new.name = names(combined.columns)[i], elim.old = elim.old)
    })
    return(dataframe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
