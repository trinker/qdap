\name{cm_csv.temp}
\alias{cm_csv.temp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Breaks transcript dialogue into words while retaining the demographic factors associate with each word
}
\description{
Breaks transcript dialogue into words while retaining the demographic factors associate with each word.  The codes argument provides a matrix of zeros that can serve as a dummy coded matrix of codes per word.
}
\usage{
wordSplit(dataframe, text.var, codes = NULL, csv = FALSE, file.name = NULL, strip = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{codes}{
%%     ~~Describe \code{codes} here~~
}
  \item{csv}{
%%     ~~Describe \code{csv} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{strip}{
%%     ~~Describe \code{strip} here~~
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
function (dataframe, text.var, codes = NULL, csv = FALSE, file.name = NULL, 
    strip = FALSE) 
{
    tv <- as.character(dataframe[, text.var])
    if (strip) {
        tv <- strip(tv)
    }
    wrds <- lapply(tv, function(x) Trim(unlist(strsplit(x, " "))))
    lens <- sapply(wrds, length)
    leftover <- dataframe[, !colnames(dataframe) \%in\% text.var, 
        drop = FALSE]
    if (!is.null(codes)) {
        lcodes <- length(codes)
        lwrds <- length(unlist(wrds))
        MAT <- matrix(rep(0, lcodes * lwrds), lwrds, lcodes)
        colnames(MAT) <- codes
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), 
            , drop = FALSE], text = unlist(wrds), MAT)
    }
    else {
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), 
            , drop = FALSE], text = unlist(wrds))
    }
    if (csv) {
        if (is.null(file.name)) {
            file.name <- as.character(substitute(dataframe))
        }
        write.table(DF, file = paste0(file.name, ".csv"), sep = ",", 
            col.names = T, row.names = F, qmethod = "double")
    }
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
