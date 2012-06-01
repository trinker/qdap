\name{qda}
\alias{qda}
\alias{print.qda}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Raw Word Lists and Frequency Counts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qda(text.var, group.vars, stopwords = NULL, cut.n = 20, cap = TRUE, cap.list = NULL, cap.I = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{group.vars}{
%%     ~~Describe \code{group.vars} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{cut.n}{
%%     ~~Describe \code{cut.n} here~~
}
  \item{cap}{
%%     ~~Describe \code{cap} here~~
}
  \item{cap.list}{
%%     ~~Describe \code{cap.list} here~~
}
  \item{cap.I}{
%%     ~~Describe \code{cap.I} here~~
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
function (text.var, group.vars, stopwords = NULL, cut.n = 20, 
    cap = TRUE, cap.list = NULL, cap.I = TRUE) 
{
    upper <- function(x) paste(substring(x, 1, 1), substring(x, 
        2, nchar(x)), sep = "")
    Sw1 <- stopwords[!substring(stopwords, 1, 1) \%in\% LETTERS]
    Sw2 <- stopwords[substring(stopwords, 1, 1) \%in\% LETTERS]
    Sw3 <- paste(Sw2, "'s", sep = "")
    stopwords <- c(Sw1, upper(Sw2), Sw2, tolower(Sw2), Sw3, tolower(Sw3))
    group.var <- if (is.list(group.vars) & length(group.vars) > 
        1) {
        apply(data.frame(group.vars), 1, function(x) {
            if (any(is.na(x))) {
                NA
            }
            else {
                paste(x, collapse = ".")
            }
        })
    }
    else {
        group.vars
    }
    NAME <- if (is.list(group.vars)) {
        m <- unlist(as.character(substitute(group.vars))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)], 
            USE.NAMES = FALSE)
        paste(m, collapse = "&")
    }
    else {
        G <- as.character(substitute(group.vars))
        G[length(G)]
    }
    word.lists1 <- textLISTER(text.var = text.var, group.vars = group.var)
    words.UNLISTED <- lapply(word.lists1, function(x) {
        y <- unlist(x)
        names(y) <- NULL
        return(y)
    })
    if (cap) {
        word.lists2 <- lapply(word.lists1, function(x) {
            y <- capitalizer(x, caps.list = cap.list)
            names(y) <- NULL
            return(y)
        })
    }
    else {
        word.lists2 <- lapply(word.lists1, function(x) {
            y <- unlist(x)
            names(y) <- NULL
            return(y)
        })
    }
    naomit <- function(x) x[!is.na(x)]
    word.lists2 <- lapply(word.lists2, naomit)
    stopped.word.list <- lapply(words.UNLISTED, function(x) {
        x[!x \%in\% stopwords]
    })
    stopped.word.list <- lapply(stopped.word.list, naomit)
    stopped.word.list <- lapply(stopped.word.list, function(x) {
        capitalizer(x, caps.list = cap.list)
    })
    COUNT <- function(x) {
        DF <- data.frame(table(x))
        names(DF) <- c("WORD", "FREQ")
        DF$WORD <- as.character(DF$WORD)
        DF$FREQ <- as.numeric(DF$FREQ)
        DF <- DF[rev(order(DF$FREQ)), ]
        rownames(DF) <- NULL
        return(DF)
    }
    freq.word.list <- lapply(word.lists2, COUNT)
    freq.stop.word.list <- lapply(stopped.word.list, COUNT)
    red.freq.stop.word.list <- ncutWORDS(freq.stop.word.list, 
        cut.n = cut.n)
    word.lists2 <- lapply(word.lists2, function(x) {
        comment(x) <- "bagOwords"
        return(x)
    })
    stopped.word.list <- lapply(stopped.word.list, function(x) {
        comment(x) <- "bagOwords"
        return(x)
    })
    freq.word.list <- lapply(freq.word.list, function(x) {
        comment(x) <- "freqList"
        return(x)
    })
    freq.stop.word.list <- lapply(freq.stop.word.list, function(x) {
        comment(x) <- "freqList"
        return(x)
    })
    red.freq.stop.word.list <- lapply(red.freq.stop.word.list, 
        function(x) {
            comment(x) <- "freqList"
            return(x)
        })
    comment(word.lists2) <- "cwl"
    comment(stopped.word.list) <- "swl"
    comment(freq.word.list) <- "fwl"
    comment(freq.stop.word.list) <- "fswl"
    comment(red.freq.stop.word.list) <- "rfswl"
    o <- list(cwl = word.lists2, swl = stopped.word.list, fwl = freq.word.list, 
        fswl = freq.stop.word.list, rfswl = red.freq.stop.word.list)
    class(o) <- "qda"
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
