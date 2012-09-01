\name{abbreviation_replace}
\alias{abbreviation_replace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replace Abbreviated Words
}
\description{
Replaces abbreviated words in preparation for sentSplit.
}
\usage{
abbreviation_replace(text.var, abbreviation = NULL, replace = NULL, ignore.case = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{abbreviation}{
%%     ~~Describe \code{abbreviation} here~~
}
  \item{replace}{
%%     ~~Describe \code{replace} here~~
}
  \item{ignore.case}{
%%     ~~Describe \code{ignore.case} here~~
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
function (text.var, abbreviation = NULL, replace = NULL, ignore.case = TRUE) 
{
    if (is.null(abbreviation)) {
        abbreviation <- abbreviations
    }
    if (!is.null(replace)) {
        ab <- data.frame(abv = abbreviation, repl = replace)
    }
    else {
        if (is.list(abbreviation)) {
            ab <- data.frame(abv = c("Mr.", "Mrs.", "Ms.", "www.", 
                ".com", "i.e.", "A.D.", "B.C.", "A.M.", "P.M.", 
                "et al."), rep = c("Mister", "Misses", "Miss", 
                "dot com", "www dot", "ie", "AD", "BC", "AM", 
                "PM", "et al"))
        }
        else {
            stop("must supply vector of abbreviations and vector of replacements")
        }
    }
    if (ignore.case) {
        ab[, 1] <- tolower(ab[, 1])
        caps <- function(string, all = FALSE) {
            capit <- function(x) paste0(toupper(substring(x, 
                1, 1)), substring(x, 2))
            if (all) {
                x <- paste(unlist(lapply(strsplit(string, " "), 
                  capit)), collapse = " ")
                y <- paste(unlist(lapply(strsplit(x, NULL), capit)), 
                  collapse = "")
                x <- c(x, y)
            }
            else {
                x <- capit(string)
            }
            return(x)
        }
        ab2 <- do.call(rbind, list(ab, ab))
        temp <- unlist(lapply(ab2[, 1], caps, TRUE))
        ab2[, 1] <- temp[1:(length(temp)/2)]
        ab <- data.frame(rbind(ab, ab2))
        ab[, 2] <- spaste(ab[, 2])
    }
    text.var <- Trim(text.var)
    pn <- which(substring(text.var, nchar(text.var)) == ".")
    text.var <- mgsub(ab[, 1], ab[, 2], text.var)
    x <- Trim(gsub("\\s+", " ", text.var))
    x[pn] <- sapply(x[pn], function(z) {
        if (substring(z, nchar(z)) != ".") {
            paste(z, ".", sep = "")
        }
    }, USE.NAMES = FALSE)
    return(scrubber(x))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
