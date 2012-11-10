\name{multispeaker}
\alias{multispeaker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multi_speaker(dataframe, person.var = 1, sep = c("and", "&", ","), track.reps = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{person.var}{
%%     ~~Describe \code{person.var} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{track.reps}{
%%     ~~Describe \code{track.reps} here~~
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
function (dataframe, person.var = 1, sep = c("and", "&", ","), 
    track.reps = FALSE) 
{
    if (!is.numeric(person.var)) {
        person.var <- which(colnames(dataframe) \%in\% person.var)
    }
    locs <- which(rowSums(termco.a(dataframe[, person.var], seq_along(dataframe[, 
        person.var]), sep, char.keep = sep)[[1]][, -c(1:2)]) > 
        0)
    newp <- lapply(dataframe[locs, person.var], function(x) {
        unblanker(Trim(unlist(strsplit(x, paste(paste0("\\", 
            sep), collapse = "|")))))
    })
    lens <- unlist(lapply(newp, length))
    expa <- rep(1, nrow(dataframe))
    expa[locs] <- lens
    DF <- dataframe[rep(seq_len(dim(dataframe)[1]), expa), ]
    unlist(lapply(seq_along(locs), function(i) {
        (locs[i] + cumsum(lens)[i]):(cumsum(lens)[i])
    }))
    w <- c(0, cumsum(lens - 1))
    a <- w[-length(w)] + locs
    b <- a + (lens - 1)
    locs2 <- unlist(lapply(seq_along(a), function(i) a[i]:b[i]))
    DF[locs2, person.var] <- unlist(newp)
    if (!track.reps) {
        rownames(DF) <- NULL
    }
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
