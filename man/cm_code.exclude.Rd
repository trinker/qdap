\name{cm_code.exclude}
\alias{cm_code.exclude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exclude Codes
}
\description{
Find the occurences of n codes excluding the nth code.  i.e.  We have times/words coded for a teacher and we also have times/words coded for happiness.  We can find all the happiness times excluding the teacher times or vise versa.
}
\usage{
cm_code.exclude(x2long.obj, exclude.code.list, rm.var = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x2long.obj}{
%%     ~~Describe \code{x2long.obj} here~~
}
  \item{exclude.code.list}{
%%     ~~Describe \code{exclude.code.list} here~~
}
  \item{rm.var}{
%%     ~~Describe \code{rm.var} here~~
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
function (x2long.obj, exclude.code.list, rm.var = NULL) 
{
    if (!is.null(rm.var)) {
        if (!rm.var \%in\% colnames(x2long.obj)) {
            stop("rm.var does not match a column")
        }
    }
    exclude.code.list1 <- exclude.code.list
    names(exclude.code.list1) <- paste0(names(exclude.code.list), 
        "_rmvme123")
    exclude.code.list2 <- invisible(lapply(seq_along(exclude.code.list), 
        function(i) {
            c(names(exclude.code.list1)[i], exclude.code.list[[i]][length(exclude.code.list[[i]])])
        }))
    names(exclude.code.list2) <- names(exclude.code.list)
    w <- cm_code.combine(x2long.obj, exclude.code.list1, rm.var = rm.var)
    z <- cm_code.blank(w, exclude.code.list2, overlap = 1, rm.var = rm.var)
    if (ncol(x2long.obj) != ncol(z)) {
        v <- x2long.obj[!colnames(x2long.obj) \%in\% colnames(z)]
        z$newname1234 <- rep(v[1, 1], nrow(z))
        colnames(z)[colnames(z) == "newname1234"] <- colnames(v)
    }
    dat <- data.frame(rbind(x2long.obj, z[z$code \%in\% names(exclude.code.list), 
        ]), row.names = NULL)
    if (!is.null(rm.var)) {
        dat[order(dat[rm.var]), ]
    }
    else {
        dat
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
