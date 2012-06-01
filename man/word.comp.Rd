\name{word.comp}
\alias{word.comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Word Use Between Two People or Same Person at Two Times
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
word.comp(x, y, wordlist = NULL, stopwords = NULL, raw.digits = 2, prop.digits = 4, alert = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{wordlist}{
%%     ~~Describe \code{wordlist} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{raw.digits}{
%%     ~~Describe \code{raw.digits} here~~
}
  \item{prop.digits}{
%%     ~~Describe \code{prop.digits} here~~
}
  \item{alert}{
%%     ~~Describe \code{alert} here~~
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
function (x, y, wordlist = NULL, stopwords = NULL, raw.digits = 2, 
    prop.digits = 4, alert = TRUE) 
{
    NAMEx <- as.character(substitute(x))
    NAMEx <- NAMEx[length(NAMEx)]
    NAMEy <- as.character(substitute(y))
    NAMEy <- NAMEy[length(NAMEy)]
    x <- qda.handler(x)
    y <- qda.handler(y)
    Dx <- data.frame(text.var = x, grouping.var = NAMEx)
    Dy <- data.frame(text.var = y, grouping.var = NAMEy)
    DF <- data.frame(rbind(Dx, Dy))
    i <- data.frame(wfm(text.var = DF$text.var, grouping.var = DF$grouping.var, 
        stopwords = stopwords))
    i$diff <- c(i[, 2] - i[, 1])
    i <- if (!is.null(stopwords)) 
        i[!rownames(i) \%in\% stopwords, ]
    prop.vect <- function(x, digits = prop.digits) round(x/sum(x), 
        digits)
    i <- transform(i, prop.diff = prop.vect(i[, 2]) - prop.vect(i[, 
        1]))
    i$perc.alert <- if (!alert) {
        NULL
    }
    else {
        x <- abs(scale(i[, "prop.diff"]))
        as.factor(ifelse(x > 4, "> 4 sd", ifelse(x > 3, "> 3 sd", 
            ifelse(x > 2, "> 2 sd", ifelse(x > 1, "> 1 sd", "-")))))
    }
    N1 <- sum(i[, 1] != 0)
    N2 <- sum(i[, 2] != 0)
    i[, "diff.effect"] <- round(c(i$diff)/sqrt(N1 + N2 + (N2 - 
        N1)), digits = raw.digits)
    i$effect.alert <- if (!alert) {
        NULL
    }
    else {
        x <- abs(scale(i[, "diff.effect"]))
        as.factor(ifelse(x > 4, "> 4 sd", ifelse(x > 3, "> 3 sd", 
            ifelse(x > 2, "> 2 sd", ifelse(x > 1, "> 1 sd", "-")))))
    }
    CH <- suppressWarnings(chisq.test(i[, names(i)[1:2]]))
    i$stdres <- round(CH$stdres[, 2], digits = raw.digits)
    i$`O-E` <- round((CH$observed - CH$expected)[, 2], digits = raw.digits)
    i$`(O-E)^2/E` <- round((((CH$observed - CH$expected)[, 2]^2)/(CH$expected[, 
        2])), digits = raw.digits)
    chi <- data.frame(do.call("cbind", CH[1:3]))
    names(chi)[1:2] <- c("chisq", "df")
    rownames(chi) <- " "
    u <- matrix(paste(NAMEy, "compared to", NAMEx, sep = " "))
    dimnames(u) <- list(c(""), c(""))
    u <- noquote(u)
    if (!is.null(wordlist)) 
        k <- i[rownames(i) \%in\% wordlist, ]
    if (!is.null(wordlist)) {
        o <- list(raw = i, `words of interest` = k, stat = chi, 
            comparison = u)
    }
    else {
        o <- list(raw = i, stat = chi, comparison = u)
    }
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
