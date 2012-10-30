\name{cm_df2long}
\alias{cm_df2long}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Codes to Start-End Durations
}
\description{
Transforms the range coding structure(s) from cm_df.temp (in list format) into a data frame of start and end durations in long format.
}
\usage{
cm_df2long(df.temp.obj, v.name = "variable", list.var = TRUE, code.vars = NULL, no.code = NA, add.start.end = TRUE, repeat.vars = NULL, rev.code = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df.temp.obj}{
%%     ~~Describe \code{df.temp.obj} here~~
}
  \item{v.name}{
%%     ~~Describe \code{v.name} here~~
}
  \item{list.var}{
%%     ~~Describe \code{list.var} here~~
}
  \item{code.vars}{
%%     ~~Describe \code{code.vars} here~~
}
  \item{no.code}{
%%     ~~Describe \code{no.code} here~~
}
  \item{add.start.end}{
%%     ~~Describe \code{add.start.end} here~~
}
  \item{repeat.vars}{
%%     ~~Describe \code{repeat.vars} here~~
}
  \item{rev.code}{
%%     ~~Describe \code{rev.code} here~~
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
function (df.temp.obj, v.name = "variable", list.var = TRUE, 
    code.vars = NULL, no.code = NA, add.start.end = TRUE, repeat.vars = NULL, 
    rev.code = FALSE) 
{
    objs <- df.temp.obj
    L1 <- lapply(objs, get)
    names(L1) <- objs
    if (is.null(code.vars)) {
        code.vars <- which(colnames(L1[[1]]) == "word.num"):ncol(L1[[1]])
    }
    L2 <- lapply(L1, function(x) {
        cm_d2l(x, code.vars = code.vars, no.code = no.code, add.start.end = add.start.end, 
            repeat.vars = repeat.vars, rev.code = rev.code)
    })
    if (list.var) {
        L2 <- lapply(seq_along(L2), function(i) data.frame(L2[[i]], 
            VAR = objs[i]))
    }
    DF <- data.frame(do.call(rbind, L2), row.names = NULL)
    if (list.var) {
        colnames(DF)[ncol(DF)] <- v.name
    }
    comment(DF) <- "cmrange"
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
