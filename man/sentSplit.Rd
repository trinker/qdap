\name{sentSplit}
\alias{sentSplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split Sentences Into Individual Lines
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sentSplit(dataframe, text.var, splitpoint = NULL, rnames = "numeric", text.place = "original")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{splitpoint}{
%%     ~~Describe \code{splitpoint} here~~
}
  \item{rnames}{
%%     ~~Describe \code{rnames} here~~
}
  \item{text.place}{
%%     ~~Describe \code{text.place} here~~
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
function (dataframe, text.var, splitpoint = NULL, rnames = "numeric", 
    text.place = "original") 
{
    DF <- dataframe
    if (length(DF) < 3) {
        DF$EXTRA1x2 <- 1:nrow(DF)
        DF$EXTRA2x2 <- 1:nrow(DF)
    }
    else {
        DF
    }
    input <- as.character(substitute(text.var))
    re <- ifelse(is.null(splitpoint), "[\\?\\.\\!]", as.character(substitute(splitpoint)))
    RN <- as.character(substitute(rnames))
    TP <- as.character(substitute(text.place))
    TP <- "original"
    breakinput <- function(input, re) {
        j <- gregexpr(re, input)
        lengths <- unlist(lapply(j, length))
        spots <- lapply(j, as.numeric)
        first <- unlist(lapply(spots, function(x) {
            c(1, (x + 1)[-length(x)])
        }))
        last <- unlist(spots)
        ans <- substring(rep(input, lengths), first, last)
        return(list(text = ans, lengths = lengths))
    }
    j <- breakinput(DF[, input], re)
    others <- DF[, -which(colnames(DF) \%in\% input), drop = FALSE]
    idx <- rep(1:dim(others)[1], j$lengths)
    ans <- data.frame(cbind(input = Trim(j$text), others[idx, 
        ]))
    colnames(ans)[1] <- input
    x <- as.character(rownames(ans))
    y <- strsplit(x, "\\.")
    z <- sapply(y, function(x) as.numeric(x[2]))
    z[is.na(z)] <- 0
    z <- as.character(z + 1)
    a <- sapply(y, function(x) x[1])
    x <- paste(a, ".", z, sep = "")
    rownames(ans) <- TOT <- x
    if (RN == "numeric") {
        rownames(ans) <- 1:nrow(ans)
    }
    if (TP == "original") {
        ans <- ans[, colnames(DF)]
    }
    else {
        if (TP == "right") {
            ans <- data.frame(ans[, -1], ans[, 1])
            colnames(ans) <- c(colnames(ans)[-ncol(ans)], input)
        }
        else {
            if (TP == "left") {
                ans
            }
        }
    }
    if (RN == "numeric") {
        ans <- data.frame(tot = TOT, ans)
    }
    ans$EXTRA1x2 <- NULL
    ans$EXTRA2x2 <- NULL
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
