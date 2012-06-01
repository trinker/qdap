\name{SMOG}
\alias{SMOG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SMOG Readability Statistic
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMOG(text.var, grouping.var = NULL, output = "valid")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
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
function (text.var, grouping.var = NULL, output = "valid") 
{
    G <- if (is.null(grouping.var)) {
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
    text <- as.character(text.var)
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    DF$word.count <- word.count(DF$text.var, missing = 0)
    i <- as.data.frame(table(DF$group))
    DF <- switch(output, valid = {
        subset(DF, group \%in\% as.character(i[i$Freq > 29, ][, 
            "Var1"]))
    }, all = DF)
    DF$group <- DF$group[, drop = TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$pollysyllable.count <- pollysyllable.sum(DF$text.var)
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$pollysyllable.count <- aggregate(pollysyllable.count ~ 
        group, DF, sum)$pollysyllable.count
    smog <- function(tse, tpsy) 1.043 * sqrt(tpsy * (30/tse)) + 
        3.1291
    DF2$SMOG <- round(with(DF2, smog(tse = sentence.count, tpsy = pollysyllable.count)), 
        digits = 1)
    DF2$validity <- ifelse(DF2$sentence.count < 30, "n < 30", 
        "valid")
    if (output == "valid") 
        DF2$validity <- NULL
    names(DF2) <- c(G, names(DF2)[-1])
    return(DF2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
