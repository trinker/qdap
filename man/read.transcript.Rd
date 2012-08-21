\name{read.transcript}
\alias{read.transcript}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read In Microsoft Excel Transcript
}
\description{
Reads in an Microsoft Excel Transcript and does some automatic cleaning.
}
\usage{
read.transcript(file, text.var = NULL, header = TRUE, dash = "", ellipsis = "...", quote2bracket = FALSE, rm.empty.rows = TRUE, col.names = NULL, sep = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{dash}{
%%     ~~Describe \code{dash} here~~
}
  \item{ellipsis}{
%%     ~~Describe \code{ellipsis} here~~
}
  \item{quote2bracket}{
%%     ~~Describe \code{quote2bracket} here~~
}
  \item{rm.empty.rows}{
%%     ~~Describe \code{rm.empty.rows} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (file, text.var = NULL, header = TRUE, dash = "", ellipsis = "...", 
    quote2bracket = FALSE, rm.empty.rows = TRUE, col.names = NULL, 
    sep = ",") 
{
    require(gdata)
    x <- gdata::read.xls(file, header = header, sep = sep, as.is = FALSE, 
        na.strings = c("999", "NA", " "), strip.white = TRUE, 
        stringsAsFactors = FALSE, blank.lines.skip = rm.empty.rows)
    if (!is.null(text.var) & !is.numeric(text.var)) {
        text.var <- which(colnames(x) == text.var)
    }
    else {
        text.col <- function(dataframe) {
            dial <- function(x) {
                if (is.factor(x) | is.character(x)) {
                  n <- max(nchar(as.character(x)))
                }
                else {
                  n <- NA
                }
            }
            which.max(unlist(lapply(dataframe, dial)))
        }
        text.var <- text.col(x)
    }
    x[, text.var] <- as.character(x[, text.var])
    x[, text.var] <- Trim(iconv(x[, text.var], "", "ASCII", "byte"))
    if (quote2bracket == TRUE) {
        rbrac <- "}"
        lbrac <- "{"
    }
    else {
        if (length(quote2bracket) == 2) {
            rbrac <- quote2bracket[2]
            lbrac <- quote2bracket[1]
        }
        else {
            lbrac <- rbrac <- ""
        }
    }
    reps <- c(lbrac, rbrac, "'", "'", "'", ellipsis, dash, dash)
    ser <- c("<e2><80><9c>", "<e2><80><9d>", "<e2><80><99>", 
        "<e2><80><9b>", "<ef><bc><87>", "<e2><80><a6>", "<e2><80><93>", 
        "<e2><80><94>")
    Encoding(x[, text.var]) <- "latin1"
    x[, text.var] <- clean(mgsub(ser, reps, x[, text.var]))
    if (rm.empty.rows) {
        x <- rm_empty_row(x)
    }
    if (!is.null(colnames)) {
        colnames(x) <- col.names
    }
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
