\name{word.freq.df}
\alias{word.freq.df}
\alias{wfdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Word Frequency Data Frame by Person
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
word.freq.df(text.var, grouping.var, stopwords = NULL, margins = FALSE, output = "raw", digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{margins}{
%%     ~~Describe \code{margins} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
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
function (text.var, grouping.var, stopwords = NULL, margins = FALSE, 
    output = "raw", digits = 2) 
{
    grouping.var <- if (is.list(grouping.var) & length(grouping.var) > 
        1) {
        apply(data.frame(grouping.var), 1, function(x) {
            if (any(is.na(x))) 
                NA
            else paste(x, collapse = ".")
        })
    }
    else {
        grouping.var
    }
    bl <- split(text.var, grouping.var)
    x <- lapply(bl, bag.o.words)
    tabs <- lapply(x, function(x) as.data.frame(table(x)))
    lapply(seq_along(tabs), function(x) {
        names(tabs[[x]]) <<- c("Words", names(tabs)[x])
    })
    DF <- merge.all(tabs, by = "Words", 0)
    DF <- DF[order(DF$Words), ]
    DF[, "Words"] <- as.character(DF[, "Words"])
    DF[, -1] <- sapply(DF[, -1], function(x) as.numeric(as.character(x)))
    if (!is.null(stopwords)) 
        DF <- DF[!DF[, "Words"] \%in\% stopwords, ]
    rownames(DF) <- 1:nrow(DF)
    pro <- function(x) x/sum(x)
    per <- function(x) 100 * (x/sum(x))
    if (output != "raw") 
        DF2 <- DF
    DF <- switch(output, raw = DF, proportion = {
        data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], pro))
    }, prop = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], 
        pro)), percent = data.frame(DF[, 1, drop = FALSE], sapply(DF[, 
        -1], per)), per = data.frame(DF[, 1, drop = FALSE], sapply(DF[, 
        -1], per)))
    if (margins) {
        if (output == "raw") {
            DF <- rbind(DF, c(NA, colSums(DF[, -1])))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
            DF[, "TOTAL.USES"] <- rowSums(DF[, -1])
        }
        else {
            X <- rowSums(DF2[, -1])
            DF[, "TOTAL.USES"] <- c(X/sum(X))
            X2 <- colSums(DF2[, -1])
            DF <- rbind(DF, c(NA, X2/sum(X), 1))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
        }
    }
    if (!output == "raw") {
        DF2 <- lapply(DF[, -1], function(x) round(x, digits = digits))
        DF <- data.frame(DF[, 1, drop = FALSE], DF2)
    }
    if (!margins & output == "raw") {
        comment(DF) <- "t.df"
    }
    else {
        if (margins & output == "raw") {
            comment(DF) <- "m.df"
        }
        else {
            comment(DF) <- "f.df"
        }
    }
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
