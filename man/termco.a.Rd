\name{termco.a}
\alias{termco.a}
\title{
Combines temco.d and termco.c into one step
}
\description{
Combines temco.d and termco.c into one step
}
\usage{
termco.a(text.var, grouping.var = NULL, match.list, ignore.case = FALSE, lazy.term = TRUE, elim.old = TRUE, zero.replace = 0, output = "percent", digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{match.list}{
%%     ~~Describe \code{match.list} here~~
}
  \item{ignore.case}{
%%     ~~Describe \code{ignore.case} here~~
}
  \item{lazy.term}{
%%     ~~Describe \code{lazy.term} here~~
}
  \item{elim.old}{
%%     ~~Describe \code{elim.old} here~~
}
  \item{zero.replace}{
%%     ~~Describe \code{zero.replace} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (text.var, grouping.var = NULL, match.list, ignore.case = FALSE, 
    lazy.term = TRUE, elim.old = TRUE, zero.replace = 0, output = "percent", 
    digits = 2) 
{
    IND <- unlist(lapply(match.list, length))
    new.names <- names(IND)[IND != 1]
    CC <- match.list[sapply(match.list, length) > 1]
    ML <- unlist(match.list)
    TD <- termco.d(text.var = text.var, grouping.var = grouping.var, 
        match.string = ML, ignore.case = ignore.case, zero.replace = zero.replace, 
        output = output, digits = digits)
    if (sd(IND) == 0) {
        o <- TD
    }
    else {
        o <- termco.c(TD, combined.columns = CC, new.name = new.names, 
            zero.replace = NULL, lazy.term = lazy.term, elim.old = elim.old)
    }
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
