\name{polarity.score}
\alias{polarity.score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply Polarity Score of Dialogue
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
polarity.score(text.var, grouping.var = NULL, pos.words = positive.words, neg.words = negative.words, negation.words = negation.words, increase.amplification.words = increase.amplification.words, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{positive.list}{
%%     ~~Describe \code{pos.words} here~~
}
  \item{negative.list}{
%%     ~~Describe \code{neg.words} here~~
}
  \item{negation.list}{
%%     ~~Describe \code{negation.words} here~~
}
  \item{amplification.list}{
%%     ~~Describe \code{increase.amplification.words} here~~
}
  \item{digits}{
%%     ~~Describe \code{increase.amplification.words} here~~
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
function (text.var, grouping.var = NULL, pos.words = positive.words, 
    neg.words = negative.words, negation.words = negation.words, 
    increase.amplification.words = increase.amplification.words) 
{
    grouping.vars <- grouping.var
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    unblank <- function(x) {
        return(x[x != ""])
    }
    no.na <- function(x) !is.na(x)
    truer <- function(x) which(!is.na(x) == TRUE)
    SEL <- function(x, y) x[y]
    score <- function(x, y) length(x) - length(y)
    add <- function(x, y, z) x + y + z
    miss <- function(x) ifelse(is.na(x), 0, x)
    NAfill <- which(is.na(text.var))
    negation <- strip(negation.words)
    amplification <- strip(increase.amplification.words)
    x <- as.data.frame(text.var)
    x$words <- unblank(word.split(strip(text.var)))
    x$wc <- word.count(text.var)
    pos.matchesPOS <- lapply(x$words, function(x) match(x, pos.words))
    neg.matchesPOS <- lapply(x$words, function(x) match(x, neg.words))
    pos <- lapply(pos.matchesPOS, no.na)
    neg <- lapply(neg.matchesPOS, no.na)
    pos1 <- lapply(pos.matchesPOS, truer)
    neg1 <- lapply(neg.matchesPOS, truer)
    x$raw <- mapply(score, pos1, neg1, SIMPLIFY = FALSE)
    x$pos.words <- mapply(SEL, x$words, pos1, SIMPLIFY = FALSE)
    x$neg.words <- mapply(SEL, x$words, neg1, SIMPLIFY = FALSE)
    L1 <- mapply(function(x, y) x[y - 1], x$words, pos1, SIMPLIFY = FALSE)
    L2 <- mapply(function(x, y) x[y - 1], x$words, neg1, SIMPLIFY = FALSE)
    x$pos.matchesNEG <- lapply(L1, function(x) sum(no.na(match(x, 
        negation))) * (-2))
    x$neg.matchesNEG <- lapply(L2, function(x) sum(no.na(match(x, 
        negation)) * 2))
    x$pos.matchesAMP <- lapply(L1, function(x) no.na(match(x, 
        amplification)))
    x$neg.matchesAMP <- lapply(L2, function(x) no.na(match(x, 
        amplification)))
    ans <- list()
    for (i in 1:nrow(x)) {
        ans[[i]] <- numeric(0)
        for (j in 1:length(x[[i, "neg.matchesAMP"]])) {
            ans[[i]][j] <- ifelse(x$neg.matchesAMP[[i]][j], as.numeric(1/(x[i, 
                "wc"] - 1)), 0)
        }
    }
    AMPneg <- lapply(ans, function(x) sum(miss(x)) * -1)
    ans2 <- list()
    for (i in 1:dim(x)[1]) {
        ans2[[i]] <- numeric(0)
        for (j in 1:length(x[[i, "pos.matchesAMP"]])) {
            ans2[[i]][j] <- ifelse(x$pos.matchesAMP[[i]][j], 
                as.numeric(1/(x[i, "wc"] - 1)), 0)
        }
    }
    AMPpos <- lapply(ans2, function(x) sum(miss(x)))
    x$negation.adj.raw <- mapply(add, x$raw, x$pos.matchesNEG, 
        x$neg.matchesNEG)
    x$amplification.adj.raw <- mapply(add, x$negation.adj.raw, 
        AMPneg, AMPpos)
    x$polarity <- round(x$amplification.adj.raw/wc(x$words), 
        digits = 3)
    x <- x[, c("text.var", "wc", "polarity", "raw", "negation.adj.raw", 
        "amplification.adj.raw", "pos.words", "neg.words")]
    x[NAfill, 1:8] <- NA
    x2 <- x
    TX <- as.character(substitute(text.var))
    TX <- TX[length(TX)]
    grouping.vars <- if (is.list(grouping.var) & length(grouping.var) > 
        1) {
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
        grouping.var
    }
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
        paste(m, collapse = "&")
    }
    else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    x <- if (is.null(grouping.var)) {
        names(x)[1] <- c(TX)
        x
    }
    else {
        x <- data.frame(grouping.vars, x)
        names(x)[1:2] <- c(NAME, TX)
        x
    }
    if (is.null(grouping.var)) {
        return(x)
    }
    else {
        DF <- data.frame(group = grouping.vars, x2[, c("text.var", 
            "wc", "polarity")])
        DF2 <- aggregate(wc ~ group, DF, sum)
        DF2$total.sentences <- as.data.frame(table(DF$group))$Freq
        DF2$ave.polarity <- round(aggregate(polarity ~ group, 
            DF, mean)$polarity, digits = 3)
        names(DF2)[names(DF2) == "wc"] <- "total.words"
        names(DF2)[1] <- NAME
        DF2 <- DF2[order(-DF2$ave.polarity), ]
        rownames(DF2) <- 1:nrow(DF2)
        DF2 <- DF2[, c(1, 3, 2, 4)]
        list(POLARITY_FOR_ALL_SENTENCES = x, POLARITY_BY_GROUP = DF2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
