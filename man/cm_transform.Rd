\name{cm_transform}
\alias{cm_transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Takes a transformed code.matrix and ``untransforms'' it to a matrix
}
\description{
Takes a transformed code.matrix and ``untransforms'' it to a matrix with correct column classes.
}
\usage{
transform.code.matrix(dataframe, text.var, code.vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{code.vars}{
%%     ~~Describe \code{code.vars} here~~
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
function (dataframe, text.var, code.vars) 
{
    DF <- data.frame(t(dataframe))
    if (!is.numeric(text.var)) {
        text.var <- which(colnames(DF) == text.var)
    }
    if (!is.numeric(code.vars)) {
        code.vars <- which(colnames(DF) \%in\% c(code.vars))
    }
    left.overs <- which(!1:ncol(DF) \%in\% c(code.vars, text.var))
    DF[, text.var] <- as.character(DF[, text.var])
    lapply(code.vars, function(i) {
        DF[, i] <<- as.numeric(DF[, i])
        return(DF)
    })
    lapply(left.overs, function(i) {
        DF[, i] <<- as.factor(DF[, i])
        return(DF)
    })
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
