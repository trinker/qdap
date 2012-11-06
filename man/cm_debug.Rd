\name{cm_debug}
\alias{cm_debug}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Debug Time Span Coding
}
\description{
Debug a list of time span codes for cm_time2long
}
\usage{
cm_debug(cm.list.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cm.list.obj}{
%%     ~~Describe \code{cm.list.obj} here~~
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
function (cm.list.obj) 
{
    debug <- function(string) {
        if (length(Trim(string)) == 1 && Trim(string) == "") {
            return(NULL)
        }
        tester <- function(x, y) {
            p <- suppressWarnings(unlist(gregexpr(x, y, fixed = TRUE)))
            j <- suppressWarnings(if (is.na(str) | length(p) == 
                1 & p < 1) {
                0
            }
            else {
                length(p)
            })
            return(j)
        }
        cols <- sapply(string, tester, x = ":") > 1
        pers <- sapply(string, tester, x = ".") < 1
        v <- unlist(strsplit(gsub(",", "", string), ":"))
        w <- strsplit(v, "\\.")
        lens <- unlist(lapply(w, length))
        m2 <- lapply(seq_along(w), function(i) {
            if (lens[i] == 1) {
                m <- c("00", "00", w[[i]])
            }
            if (lens[i] == 2) {
                m <- c("00", w[[i]])
            }
            else {
                m <- w[[i]]
            }
            return(m)
        })
        d1 <- do.call(rbind, lapply(m2, as.numeric))
        inc <- c(FALSE, diff(d1[, 1] * 60 * 60 + d1[, 2] * 60 + 
            d1[, 3]) < 0)
        names(inc) <- paste2(do.call(rbind, m2))
        if (any(pers)) {
            inc <- NULL
        }
        list(COLON = cols, PERIOD = pers, INCREASE = inc)
    }
    LIST <- unlist(lapply(cm.list.obj, debug), recursive = FALSE)
    LIST <- LIST[!sapply(LIST, is.null)]
    L2 <- LIST[sapply(LIST, any)]
    L3 <- lapply(L2, function(x) names(x[x]))
    if (identical(as.character(L3), character(0))) {
        NULL
    }
    else {
        L3
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
