\name{cm_df.fill}
\alias{cm_df.fill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Range Coding of a splitWord Matrix
}
\description{
Allows range coding of words for efficient coding.
}
\usage{
insertDummy(dataframe, ranges, value = 1, text.var = NULL, code.vars = NULL, transform = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{ranges}{
%%     ~~Describe \code{ranges} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{code.vars}{
%%     ~~Describe \code{code.vars} here~~
}
  \item{transform}{
%%     ~~Describe \code{transform} here~~
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
function (dataframe, ranges, value = 1, text.var = NULL, code.vars = NULL, 
    transform = FALSE) 
{
    if (transform) {
        dataframe <- transform.code.matrix(dataframe = dataframe, 
            text.var = text.var, code.vars = code.vars)
    }
    if (!is.null(text.var) && !is.numeric(text.var)) {
        text.var <- which(colnames(DF) == text.var)
    }
    if (!is.null(code.vars) && !is.numeric(code.vars)) {
        code.vars <- which(colnames(DF) \%in\% c(code.vars))
    }
    if (is.null(text.var)) {
        text.var <- which(colnames(dataframe) == "text")
    }
    if (is.null(code.vars)) {
        code.vars <- (text.var + 2):ncol(dataframe)
    }
    left.overs <- which(!1:ncol(dataframe) \%in\% c(code.vars, 
        text.var))
    dataframe[, text.var] <- as.character(dataframe[, text.var])
    if (length(value) == 1) {
        value <- rep(value, length(code.vars))
    }
    CV <- dataframe[, code.vars]
    lapply(1:ncol(CV), function(i) {
        CV[ranges[[colnames(CV)[i]]], colnames(CV)[i]] <<- value[i]
        return(CV)
    })
    DF <- data.frame(dataframe[, 1:(text.var + 1)], CV, stringsAsFactors = FALSE)
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
