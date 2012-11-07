\name{cm_df.transcript}
\alias{cm_df.transcript}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cm_df.transcript(text.var, grouping.var, txt.file = NULL, indent = 4, width = 80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{txt.file}{
%%     ~~Describe \code{txt.file} here~~
}
  \item{indent}{
%%     ~~Describe \code{indent} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
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
function (text.var, grouping.var, txt.file = NULL, indent = 4, 
    width = 80) 
{
    if (is.list(grouping.var)) {
        grouping.var <- paste2(grouping.var)
    }
    L2 <- sentCombine(text.var, grouping.var)
    DF <- data.frame(group = names(L2), text = unlist(L2))
    DF2 <- cm_df.temp(DF, "text")
    y <- rle(as.character(DF2$group))
    lens <- y$lengths
    group <- y$values
    x <- cumsum(lens)
    L3 <- split(DF2, as.factor(rep(seq_along(x), lens)))
    invisible(lapply(seq_along(L3), function(i) {
        numbtext(L3[[i]][, "text"], width = width, lengths = L3[[i]][, 
            "word.num"], txt.file = txt.file, name = group[i], 
            indent = indent)
    }))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
