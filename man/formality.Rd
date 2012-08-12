\name{formality}
\alias{formality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Formality statistic
}
\description{
Formality statistic
}
\usage{
formality(text.var, grouping.var = NULL, sort.by.formality = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{sort.by.formality}{
%%     ~~Describe \code{sort.by.formality} here~~
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
Heylighen, F., & Dewaele, J.M. (2002). Variation in the contextuality of language: An
    empirical measure. Context in Context, Special issue of Foundations of Science, 7 (3),
    293-340.
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
function (text.var, grouping.var = NULL, sort.by.formality = TRUE) 
{
    x <- "\n\nHeylighen, F., & Dewaele, J.-M. (2002). Variation in the contextuality of language: an\n        empirical measure. Context in Context, Special issue of Foundations of Science, 7 (3),\n        293<96>340.\n\n\n\n\n"
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
    X <- pos.by(text.var = text.var, grouping.var = grouping.var)
    nameX <- rownames(X)
    X <- data.frame(X)
    xn <- nrow(X)
    X$JI <- rep(0, xn)
    X$JK <- rep(0, xn)
    DF1 <- data.frame(noun = rowSums(X[, names(X) \%in\% c("NN", 
        "NNS", "NNP", "NNPS", "POS", "JI", "JK")]), verb = rowSums(X[, 
        names(X) \%in\% c("MD", "VB", "VBD", "VBG", "VBN", "VBP", 
            "VBZ", "JI", "JK")]), adverb = rowSums(X[, names(X) \%in\% 
        c("RB", "RBR", "RBS", "WRB", "JI", "JK")]), pronoun = rowSums(X[, 
        names(X) \%in\% c("PRP", "PRP.", "WDT", "WP", "WP.", "JI", 
            "JK")]), prep = rowSums(X[, names(X) \%in\% c("IN", 
        "RP", "TO", "JI", "JK")]), adj = rowSums(X[, names(X) \%in\% 
        c("CD", "DT", "JJ", "JJR", "JJS", "JI", "JK")]), interj = rowSums(X[, 
        names(X) \%in\% c("UH", "JI", "JK")]))
    FOR <- (rowSums(cbind(DF1$noun, DF1$adj, DF1$prep)) - rowSums(cbind(DF1$pronoun, 
        DF1$verb, DF1$adverb, DF1$interj)) + 100)/2
    if (!is.null(grouping.var)) {
        FOR <- data.frame(replace = X[, 1], formality = FOR)
        colnames(FOR)[1] <- G
    }
    if (sort.by.formality) {
        FOR <- FOR[order(-FOR$formality), ]
        rownames(FOR) <- NULL
    }
    return(FOR)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
