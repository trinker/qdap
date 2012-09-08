\name{gantt_plot}
\alias{gantt_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gantt Plots
}
\description{
A convenience that wraps gantt, gantt_rm and gantt_wrap into a single plotting function.
}
\usage{
gantt_plot(text.var, grouping.var, rm.var = NULL, units = "words", col.sep = "_", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{rm.var}{
%%     ~~Describe \code{rm.var} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
  \item{col.sep}{
%%     ~~Describe \code{col.sep} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (text.var, grouping.var, rm.var = NULL, units = "words", 
    col.sep = "_", ...) 
{
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
        paste(m, collapse = col.sep)
    }
    else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    if (is.null(rm.var)) {
        x <- gantt(text.var = text.var, grouping.var = grouping.var, 
            plot = FALSE, units = units, col.sep = col.sep)
        colnames(x)[1] <- NAME
    }
    else {
        rmNAME <- if (is.list(rm.var)) {
            m <- unlist(as.character(substitute(rm.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
            paste(m, collapse = col.sep)
        }
        else {
            G <- as.character(substitute(rm.var))
            G[length(G)]
        }
        x <- gantt_rep(rm.var = rm.var, text.var = text.var, 
            grouping.var = grouping.var, units = units, col.sep = col.sep)
        colnames(x)[1:2] <- c(rmNAME, NAME)
    }
    if (is.null(rm.var)) {
        rmNAME <- NULL
    }
    if (length(rm.var) == 2) {
        x <- colsplit2df(x, sep = ".")
        rmNAME <- unlist(strsplit(rmNAME, col.sep, fixed = TRUE))
        colnames(x)[1:2] <- rmNAME
    }
    y <- gantt_wrap(dataframe = x, plot.var = NAME, facet.vars = rmNAME, 
        ...)
    invisible(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
