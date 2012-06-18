\name{stemmer}
\alias{stemmer}
Stems a vector of text strings
\title{
%%  ~~function to do ... ~~
}
\description{
Stems a vector of text strings
}
\usage{
stemmer(text.var, rm.bracket = TRUE, capitalize = TRUE, warn = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{rm.bracket}{
%%     ~~Describe \code{rm.bracket} here~~
}
  \item{capitalize}{
%%     ~~Describe \code{capitalize} here~~
}
  \item{warn}{
%%     ~~Describe \code{warn} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (text.var, rm.bracket = TRUE, capitalize = TRUE, warn = TRUE, 
    ...) 
{
    require(tm)
    txt <- as.character(text.var)
    if (rm.bracket) {
        txt <- bracketX(txt)
    }
    nc <- nchar(txt)
    end.mark1 <- substring(txt, nc, nc)
    end.mark2 <- substring(txt, nc - 1, nc)
    sent.type <- ifelse(end.mark2 \%in\% c("*.", "*?", "*!", "*|"), 
        end.mark2, ifelse(end.mark1 \%in\% c(".", "?", "!", "|"), 
            end.mark1, ifelse(is.na(end.mark1), NA, "")))
    bl <- sent.type == ""
    if (any(na.omit(bl)) & warn) {
        warning(paste("The following row(s) do have standard qdap punctuation endmarks:\n", 
            " rows:", which(bl)))
    }
    LIST <- stopwords(txt, stopwords = NULL, strip = TRUE)
    LIST <- lapply(LIST, function(x) ifelse(is.na(x), NA, tm::stemDocument(x)))
    if (capitalize) {
        LIST <- lapply(LIST, function(x) capitalizer(x, ...))
    }
    if (length(text.var) == 1) {
        if (!is.na(text.var)) {
            LIST <- paste2(LIST, sep = " ")
        }
    }
    txt2 <- paste2(list(lapply(LIST, paste2, sep = " "), sent.type), 
        sep = "")
    if (capitalize) {
        capit <- function(string) {
            if (is.na(string)) {
                NA
            }
            else {
                nc <- nchar(string)
                paste0(toupper(substr(string, 1, 1)), substr(string, 
                  2, nc))
            }
        }
        txt2 <- unlist(lapply(Trim(txt2), capit))
    }
    return(txt2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
