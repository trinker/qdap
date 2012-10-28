\name{cm_r2l}
\alias{cm_r2l}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Codes to Start-End Times
}
\description{
A helper function for cm_range2long that transforms the range coding structure from cm_range.temp (in list format) into a data frame of start and end times in long format.
}
\usage{
cm_r2l(range.list, v.name = "variable", list.var = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{range.list}{
%%     ~~Describe \code{range.list} here~~
}
  \item{v.name}{
%%     ~~Describe \code{v.name} here~~
}
  \item{list.var}{
%%     ~~Describe \code{list.var} here~~
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
function (range.list, v.name = "variable", list.var = TRUE) 
{
    lv <- as.character(substitute(range.list))
    range.list <- range.list[sapply(range.list, function(x) all(Trim(x) != 
        ""))]
    bef <- sapply(range.list, length, USE.NAMES = FALSE)
    aft <- sapply(range.list, function(x) length(unlist(strsplit(x, 
        ":"))), USE.NAMES = FALSE)
    check <- unlist(aft > bef)
    if (any(check)) {
        inds <- which(check)
        append2 <- function(x, y = ":", z) {
            lapply(z, function(z) {
                x <<- append(x, y, after = z)
            })
            x
        }
        NM <- names(range.list)
        lapply(inds, function(i) {
            inds2 <- which(grepl(":", unlist(range.list[i]))) - 
                1
            new <- unlist(strsplit(unlist(range.list[i]), ":"))
            constant <- (seq_along(inds2) - 1) * 2
            x1 <- gsub(",", "", append2(new, z = c(inds2 + constant)))
            names(x1) <- NULL
            range.list[[i]] <<- x1
        })
        names(range.list) <- NM
    }
    colon <- function(x) which(x == ":")
    ncolon <- function(x) x != ":"
    x <- range.list
    COL <- lapply(x, colon)
    Wcol <- lapply(COL, function(x) -1 + sort(x + rep(1:2, each = length(x))))
    COLneg <- lapply(x, ncolon)
    x <- lapply(seq_along(x), function(i) {
        x[[i]][unlist(COLneg[i])]
    })
    append2 <- function(x, y = ":", z) {
        lapply(z, function(z) {
            x <<- append(x, y, after = z)
        })
        x
    }
    x2 <- lapply(seq_along(x), function(n) append2(x[[n]], z = COL[[n]]))
    x3 <- lapply(x2, function(v) {
        if (!any(v == ":")) {
            dat <- data.frame(matrix(rep(v, each = 2), byrow = TRUE, 
                ncol = 2))
            colnames(dat) <- c("start", "end")
            dat
        }
        else {
            if (sum(v == ":") & length(v) == 3) {
                v <- v[v != ":"]
                dat <- data.frame(rbind(v, c(NA, NA)), row.names = NULL)
                colnames(dat) <- c("start", "end")
                dat
            }
            else {
                data.frame(start = v[-c(f <- which(v == ":"), 
                  f + 1)], end = v[-c(f, f - 1)], stringsAsFactors = FALSE)
            }
        }
    })
    x3 <- lapply(seq_along(x3), function(i) {
        data.frame(x3[[i]], varaible = rep(names(range.list)[i], 
            nrow(x3[[i]])))
    })
    dat <- data.frame(do.call(rbind, x3), row.names = NULL)
    DF <- dat[!is.na(dat[, 1]), ]
    invisible(lapply(1:2, function(i) {
        DF[, i] <<- as.numeric(as.character(DF[, i]))
    }))
    DF[, 1] <- DF[, 1] - 1
    if (list.var) {
        names(DF)[ncol(DF)] <- v.name
    }
    else {
        DF[, ncol(DF)] <- NULL
    }
    comment(DF) <- "cmrange"
    DF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
