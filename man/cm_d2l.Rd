\name{cm_d2l}
\alias{cm_d2l}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape a code matrix or data frame to a long data frame 
}
\description{
Reshape a code matrix or data frame to a long data frame.  The code matrix is reshaped into a single factor column.  Useful for plotting including gantt plot with ggplot2.
}
\usage{
code2long(dataframe, code.vars, repeat.vars = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{code.vars}{
%%     ~~Describe \code{code.vars} here~~
}
  \item{repeat.vars}{
%%     ~~Describe \code{repeat.vars} here~~
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
function (dataframe, code.vars, repeat.vars = NULL) 
{
    if (is.numeric(code.vars)) {
        code.vars <- colnames(dataframe)[code.vars]
    }
    A <- paste2(dataframe[, code.vars])
    B <- lapply(strsplit(A, "\\."), function(x) as.logical(as.numeric(x)))
    D <- lapply(B, function(x) code.vars[x])
    if (is.null(repeat.vars)) {
        repeat.vars <- colnames(dataframe)[!c(text.var, codes) \%in\% 
            colnames(dataframe)]
    }
    else {
        if (is.numeric(repeat.vars)) {
            repeat.vars <- colnames(dataframe)[repeat.vars]
        }
    }
    E <- dat[, repeat.vars, drop = FALSE]
    lens <- sapply(D, length)
    NEW <- data.frame(code = unlist(D), E[rep(1:nrow(E), lens), 
        ])
    rownames(NEW) <- NULL
    return(NEW)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
