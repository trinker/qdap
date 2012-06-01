\name{syllable.count}
\alias{syllable.count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count the Number of Syllables Per Word in a Text String
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
syllable.count(text, remove.bracketed = TRUE, algorithm.report = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{remove.bracketed}{
%%     ~~Describe \code{remove.bracketed} here~~
}
  \item{algorithm.report}{
%%     ~~Describe \code{algorithm.report} here~~
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
function (text, remove.bracketed = TRUE, algorithm.report = FALSE) 
{
    if (is.na(text)) {
        NA
    }
    else {
        q <- gsub("\n", " ", text)
        q <- gsub("\t", " ", q)
        q <- if (remove.bracketed == TRUE) {
            bracketX(q)
        }
        else {
            q
        }
        q <- strip(q)
        q <- gsub("-", " ", q)
        q <- gsub("—", " ", q)
        q <- gsub(" +", " ", q)
        q <- c(sapply(q, function(x) as.vector(unlist(strsplit(x, 
            " ")))))
        y <- tolower(q)
        SYLL <- function(x) {
            if (exists(x, env = env)) {
                return(get(x, e = env))
            }
            else {
                x2 <- as.character(substring(x, 1, nchar(x) - 
                  1))
                if (substring(x, nchar(x), nchar(x)) == "s" & 
                  exists(x2, env = env)) {
                  return(get(x2, e = env))
                }
                else {
                  m <- gsub("eeing", "XX", x)
                  m <- gsub("eing", "XX", m)
                  ended <- function(z) {
                    if (substring(z, nchar(z) - 1, nchar(z)) == 
                      "ed" & substring(z, nchar(z) - 2, nchar(z) - 
                      2) \%in\% c("t", "d")) {
                      z
                    }
                    else {
                      if (substring(z, nchar(z) - 1, nchar(z)) == 
                        "ed" & !substring(z, nchar(z) - 2, nchar(z) - 
                        2) \%in\% c("t", "d")) {
                        substring(z, 1, nchar(z) - 2)
                      }
                      else {
                        z
                      }
                    }
                  }
                  m <- ended(m)
                  conely <- function(z) {
                    if (substring(z, nchar(z) - 2, nchar(z)) == 
                      "ely") {
                      paste(substring(z, 1, nchar(z) - 3), "ly", 
                        sep = "")
                    }
                    else {
                      z
                    }
                  }
                  m <- conely(m)
                  conle <- function(z) {
                    if (substring(z, nchar(z) - 1, nchar(z)) == 
                      "le" & !substring(z, nchar(z) - 2, nchar(z) - 
                      2) \%in\% c("a", "e", "i", "o", "u", "y")) {
                      paste(substring(z, 1, nchar(z) - 1), "X", 
                        sep = "")
                    }
                    else {
                      if (substring(z, nchar(z) - 1, nchar(z)) == 
                        "le" & substring(z, nchar(z) - 2, nchar(z) - 
                        2) \%in\% c("a", "e", "i", "o", "u", "y")) {
                        substring(z, 1, nchar(z) - 1)
                      }
                      else {
                        z
                      }
                    }
                  }
                  m <- conle(m)
                  conles <- function(z) {
                    if (substring(z, nchar(z) - 2, nchar(z)) == 
                      "les" & !substring(z, nchar(z) - 3, nchar(z) - 
                      3) \%in\% c("a", "e", "i", "o", "u", "y")) {
                      paste(substring(z, 1, nchar(z) - 2), "X", 
                        sep = "")
                    }
                    else {
                      if (substring(z, nchar(z) - 2, nchar(z)) == 
                        "les" & substring(z, nchar(z) - 3, nchar(z) - 
                        3) \%in\% c("a", "e", "i", "o", "u", "y")) {
                        substring(z, 1, nchar(z) - 2)
                      }
                      else {
                        z
                      }
                    }
                  }
                  m <- conles(m)
                  magice <- function(z) {
                    if (substring(z, nchar(z), nchar(z)) == "e" & 
                      length(intersect(unlist(strsplit(z, NULL)), 
                        c("a", "e", "i", "o", "u", "y"))) > 1) {
                      substring(z, 1, nchar(z) - 1)
                    }
                    else {
                      z
                    }
                  }
                  m <- magice(m)
                  nchar(gsub("[^X]", "", gsub("[aeiouy]+", "X", 
                    m)))
                }
            }
        }
        n <- sapply(y, function(x) SYLL(x))
        InDic <- function(x) ifelse(exists(x, env = env), "-", 
            "NF")
        k <- sapply(y, InDic)
        DF <- data.frame(words = q, syllables = n, in.dictionary = k)
        row.names(DF) <- 1:nrow(DF)
        if (algorithm.report == TRUE) {
            list(`ALGORITHM REPORT` = DF[which(DF$in.dictionary == 
                "NF"), ], `SYLLABLE DATAFRAME` = DF)
        }
        else {
            DF
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
