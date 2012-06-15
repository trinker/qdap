\name{adjacency_matrix}
\alias{adjacency_matrix}
\alias{adjmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Takes a matrix and generates an adjacency matrix
}
\description{
Takes a matrix (wfm) or termco object (.c or .d) and generates an adjacency matrix for use with igraph
}
\usage{
adjacency_matrix(matrix.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix.obj}{
%%     ~~Describe \code{matrix.obj} here~~
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
function (matrix.obj) 
{
    if (class(matrix.obj) \%in\% c("termco_d", "termco_c")) {
        info <- matrix.obj
        if (matrix.obj[["zero_replace"]] != 0) {
            matrix.obj <- replacer(matrix.obj[["raw"]], matrix.obj[["zero_replace"]], 
                0)
        }
        else {
            matrix.obj <- matrix.obj[["raw"]]
        }
        matrix.obj <- termco2mat(matrix.obj)
    }
    else {
        if (class(matrix.obj) == "matrix") {
            if (is.null(comment(matrix.obj))) {
                warning("Not a termco or wfm object; results may not be correct.")
            }
            else {
                if (comment(matrix.obj) != "true.matrix") {
                  warning(paste("Not a termco.d, termco.c or wfm object;", 
                    "results may not be correct."))
                }
            }
        }
        else {
            warning("Not a matrix object; results may not be correct.")
        }
    }
    Y <- matrix.obj >= 1
    Y <- apply(Y, 2, as, "numeric")
    rownames(Y) <- rownames(matrix.obj)
    Z <- Z2 <- t(Y) \%*\% Y
    Z2[!lower.tri(Z2)] <- NA
    Z2 <- Z2[-1, -ncol(Z2)]
    o <- list(boolean = Y, adjacency = Z, shared = Z2, sums = colSums(Y))
    class(o) <- "adjacency.matrix"
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
