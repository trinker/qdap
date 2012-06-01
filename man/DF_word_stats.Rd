\name{DF_word_stats}
\alias{DF_word_stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Descriptive Word Statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DF_word_stats(text.var, digit.remove = FALSE, apostrophe.remove = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{digit.remove}{
%%     ~~Describe \code{digit.remove} here~~
}
  \item{apostrophe.remove}{
%%     ~~Describe \code{apostrophe.remove} here~~
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
function (text.var, digit.remove = FALSE, apostrophe.remove = FALSE) 
{
    text <- as.character(text.var)
    DF <- na.omit(data.frame(text.var = text, stringsAsFactors = FALSE))
    DF$n.sent <- 1:nrow(DF)
    DF$word.count <- word.count(DF$text.var, missing = 0, digit.remove = digit.remove)
    DF$character.count <- character.count(DF$text.var, apostrophe = apostrophe.remove, 
        digit.remove = digit.remove)
    DF$word.count <- word.count(DF$text.var, missing = 0, digit.remove = digit.remove)
    DF <- data.frame(DF, combo_syllable.sum(DF$text.var))
    DF <- DF[, c("text.var", "n.sent", "word.count", "character.count", 
        "syllable.count", "polysyllable.count")]
    DF <- transform(DF, char2word.ratio = round(character.count/word.count, 
        digits = 3), syl2word.ratio = round(syllable.count/word.count, 
        digits = 3), polysyl2word.ratio = round(polysyllable.count/word.count, 
        digits = 3))
    punctuation <- function(x) substr(x, nchar(x), nchar(x))
    DF$end.mark <- unlist(lapply(DF$text.var, punctuation))
    rownames(DF) <- 1:nrow(DF)
    DF <- DF[order(DF$n.sent), ]
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
