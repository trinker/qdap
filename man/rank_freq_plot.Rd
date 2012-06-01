\name{rank_freq_plot}
\alias{rank_freq_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank Frequency Plot 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rank_freq_plot(words, frequencies, title.ext = NULL, plot = TRUE, jitter.ammount = 0.1, log.scale = TRUE, hap.col = "red", dis.col = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{words}{
%%     ~~Describe \code{words} here~~
}
  \item{frequencies}{
%%     ~~Describe \code{frequencies} here~~
}
  \item{title.ext}{
%%     ~~Describe \code{title.ext} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{jitter.ammount}{
%%     ~~Describe \code{jitter.ammount} here~~
}
  \item{log.scale}{
%%     ~~Describe \code{log.scale} here~~
}
  \item{hap.col}{
%%     ~~Describe \code{hap.col} here~~
}
  \item{dis.col}{
%%     ~~Describe \code{dis.col} here~~
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
function (words, frequencies, title.ext = NULL, plot = TRUE, 
    jitter.ammount = 0.1, log.scale = TRUE, hap.col = "red", 
    dis.col = "blue") 
{
    original.data <- data.frame(words = words, freq = frequencies)
    X <- tapply(words, frequencies, function(x) length(x))
    ZIPF <- data.frame(n.words = as.numeric(rev(X)), freq = as.numeric(rev(rownames(X))), 
        rank = as.numeric(rev(nrow(X):1)))
    rownames(ZIPF) <- 1:nrow(ZIPF)
    title <- if (is.null(title.ext)) 
        NULL
    else paste(c("for", title.ext), collapse = " ")
    ZIPF2 <- ZIPF[rep(seq(dim(ZIPF)[1]), ZIPF$n.words), ]
    ZIPF <- transform(ZIPF, per.of.tot = round(freq/sum(ZIPF2$freq) * 
        100, digits = 3))
    if (plot) {
        if (log.scale) {
            with(ZIPF2, plot(jitter(log(rank), amount = jitter.ammount), 
                log(freq), ylab = "Frequency (log scale)", xlab = "Rank (log scale)", 
                main = paste("Rank-Frequency Plot", title, collapse = " "), 
                col = ifelse(freq == 1, hap.col, ifelse(freq == 
                  2, dis.col, "black"))))
        }
        else {
            with(ZIPF2, plot(jitter(rank, amount = jitter.ammount), 
                freq, ylab = "Frequency", xlab = "Rank", main = paste("Rank-Frequency Plot", 
                  title, collapse = " "), col = ifelse(freq == 
                  1, hap.col, ifelse(freq == 2, dis.col, "black"))))
        }
    }
    else {
        NULL
    }
    percent_hapax_legomena <- round(ZIPF[ZIPF$freq == 1, "n.words"]/sum(ZIPF$n.words) * 
        100, digits = 3)
    percent_dis_legomena <- round(ZIPF[ZIPF$freq == 2, "n.words"]/sum(ZIPF$n.words) * 
        100, digits = 3)
    list(ORIGINAL_DATA = original.data, RANK_AND_FREQUENCY_STATS = ZIPF, 
        LEGOMENA_STATS = c(percent_hapax_legomena = percent_hapax_legomena, 
            percent_dis_legomena = percent_dis_legomena))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
