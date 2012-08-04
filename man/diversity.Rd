\name{diversity}
\alias{diversity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diversity indices of word usage 
}
\description{
Diversity indices of word usage 
}
\usage{
diversity(text.var, grouping.var = NULL, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
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
function (text.var, grouping.var = NULL, digits = 3) 
{
    NAME <- if (is.null(grouping.var)) {
        "all"
    }
    else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
            paste(m, collapse = "&")
        }
        else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    grouping <- if (is.null(grouping.var)) {
        rep("all", length(text.var))
    }
    else {
        if (is.list(grouping.var) & length(grouping.var) > 1) {
            apply(data.frame(grouping.var), 1, function(x) {
                if (any(is.na(x))) {
                  NA
                }
                else {
                  paste(x, collapse = ".")
                }
            })
        }
        else {
            unlist(grouping.var)
        }
    }
    DF <- na.omit(data.frame(group = grouping, text.var = as.character(text.var), 
        stringsAsFactors = FALSE))
    shannon <- function(num.var, digits = 3) {
        N <- sum(num.var)
        P <- num.var/N
        logP <- log(P)
        return(round(sum(-(P * logP)), digits = digits))
    }
    simpson <- function(num.var, digits = 3) {
        N <- sum(num.var)
        FUN <- function(x) x * (x - 1)
        n.sum <- sum(FUN(num.var))
        sim <- 1 - (n.sum/(N * (N - 1)))
        return(round(sim, digits = digits))
    }
    collision <- function(num.var, digits = 3) {
        N <- sum(num.var)
        P <- num.var/N
        Epi2 <- sum(P^2)
        return(round((-log(Epi2)), digits = digits))
    }
    berger_parker <- function(num.var, digits = 3) {
        N <- sum(num.var)
        Nm <- max(num.var)
        return(round(Nm/N, digits = digits))
    }
    brillouin <- function(num.var, digits = 3) {
        N <- sum(num.var)
        br <- (lfactorial(N) - sum(lfactorial(num.var)))/N
        return(round(br, digits = digits))
    }
    RICH <- function(x) {
        c(wc = sum(x), simpson = simpson(x, digits = digits), 
            shannon = shannon(x, digits = digits), collision = collision(x, 
                digits = digits), berger_parker = berger_parker(x, 
                digits = digits), brillouin = brillouin(x, digits = digits))
    }
    z <- split(DF[, "text.var"], DF[, "group"])
    y <- lapply(z, stopwords, stopwords = NULL, unlist = TRUE, 
        strip = TRUE)
    w <- lapply(y, function(x) data.frame(table(x)))
    v <- do.call(rbind, lapply(w, function(x) RICH(x[, 2])))
    o <- data.frame(rownames(v), v)
    o <- o[order(o[, 1]), ]
    rownames(o) <- NULL
    colnames(o)[1] <- NAME
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
