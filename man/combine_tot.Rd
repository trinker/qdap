\name{combine_tot}
\alias{combine_tot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine Rows With Duplicate Grouping Factor
}
\description{
Used to combine rows that contain duplicate grouping factors.
}
\usage{
combine_tot(dataframe, combine.var = 1, text.var = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{combine.var}{
%%     ~~Describe \code{combine.var} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
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
function (dataframe, combine.var = 1, text.var = 2) 
{
    NAMES <- colnames(dataframe)
    lens <- rle(as.character(dataframe[, combine.var]))
    z <- lens$lengths > 1
    z[lens$lengths > 1] <- 1:sum(lens$lengths > 1)
    a <- rep(z, lens$lengths)
    dataframe[, "ID"] <- 1:nrow(dataframe)
    b <- split(dataframe, a)
    w <- b[names(b) != "0"]
    v <- lapply(w, function(x) {
        x <- data.frame(var1 = x[1, 1], text = paste(x[, text.var], 
            collapse = " "), ID = x[1, 3])
        colnames(x)[1:2] <- NAMES
        return(x)
    })
    v$x <- as.data.frame(b["0"])
    colnames(v$x) <- unlist(strsplit(colnames(v$x), "\\."))[c(F, 
        T)]
    h <- do.call(rbind, v)
    h <- h[order(h$ID), ][, -3]
    rownames(h) <- NULL
    return(h)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
