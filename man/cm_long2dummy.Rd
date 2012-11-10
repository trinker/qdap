\name{cm_long2dummy}
\alias{cm_long2dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cm_long2dummy(dataframe, repmes = NULL, code = "code", start = "start", end = "end")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{repmes}{
%%     ~~Describe \code{repmes} here~~
}
  \item{code}{
%%     ~~Describe \code{code} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
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
function (dataframe, repmes = NULL, code = "code", start = "start", 
    end = "end") 
{
    if (!is.null(repmes)) {
        L1 <- split(dataframe, dataframe[, repmes])
    }
    else {
        L1 <- list(dataframe)
        names(L1) <- as.character(substitute(dataframe))
    }
    dummy <- function(dat, code, start, end) {
        L2 <- split(dat, dat[, code])
        inc <- function(dataframe, start, end) {
            any(diff(c(apply(dataframe[, c(start, end)], 1, c))) < 
                0)
        }
        if (any(sapply(L2, function(x) inc(x, start = start, 
            end = end)))) {
            stop("Code values not increasing.  Possible missing repmes argument.")
        }
        nr <- max(sapply(L2, function(x) max(x[, end])))
        nc <- length(L2)
        mat <- matrix(rep(0, nr * nc), ncol = nc)
        colnames(mat) <- names(L2)
        yes <- lapply(L2, function(x) {
            c(unique(unlist(sapply(1:nrow(x), function(i) x[i, 
                start]:x[i, end]))))
        })
        invisible(lapply(seq_along(yes), function(i) {
            mat[yes[[i]], i] <<- 1
        }))
        data.frame(mat)
    }
    lapply(L1, function(x) dummy(dat = x, code = code, start = start, 
        end = end))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
