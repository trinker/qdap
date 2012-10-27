\name{cm_t2l}
\alias{cm_t2l}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Codes to Start-End Times
}
\description{
Transforms the range coding structure from cm_time.temp (in list format) into a data frame of start and end times in long format.
}
\usage{
cm_time2long(time.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time.list}{
%%     ~~Describe \code{time.list} here~~
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
function (time.list) 
{
    bef <- sapply(time.list, length, USE.NAMES = FALSE)
    aft <- sapply(time.list, function(x) length(unlist(strsplit(x, 
        ":"))), USE.NAMES = FALSE)
    check <- unlist(aft > bef)
    check[1] <- FALSE
    if (any(check)) {
        inds <- which(check)
        append2 <- function(x, y = ":", z) {
            lapply(z, function(z) {
                x <<- append(x, y, after = z)
            })
            x
        }
        NM <- names(time.list)
        lapply(inds, function(i) {
            inds2 <- which(grepl(":", unlist(time.list[i]))) - 
                1
            new <- unlist(strsplit(unlist(time.list[i]), ":"))
            constant <- (seq_along(inds2) - 1) * 2
            x1 <- gsub(",", "", append2(new, z = c(inds2 + constant)))
            names(x1) <- NULL
            time.list[[i]] <<- x1
        })
        names(time.list) <- NM
    }
    reformat <- function(x) {
        colon <- grepl(":", x)
        period <- grepl("\\.", x)
        add <- colSums(rbind(colon, period)) == 0
        x[add] <- paste0(x[add], ".00")
        period <- grepl("\\.", x)
        per2 <- unlist(lapply(gregexpr("\\.", x), function(x) {
            ifelse(x < 0, 0, length(x))
        }))
        per2 <- !per2 \%in\% c(0, 2)
        FUN <- function(x) ifelse(nchar(x) < 2, paste0("0", x), 
            x)
        v <- strsplit(x[period], "\\.")
        v <- lapply(v, function(x) {
            if (length(x) == 1) {
                x <- c("00", "00", x)
            }
            if (length(x) == 2) {
                x <- c("00", x)
            }
            return(x)
        })
        x[period] <- paste2(apply(do.call(rbind, v), 2, FUN))
        x
    }
    colon <- function(x) which(x == ":")
    ncolon <- function(x) x != ":"
    x <- time.list
    x[[1]] <- suppressWarnings(gsub(":", "\\.", x[[1]][-1]))
    x <- suppressWarnings(lapply(x, reformat))
    COL <- lapply(x[-1], colon)
    Wcol <- lapply(COL, function(x) -1 + sort(x + rep(1:2, each = length(x))))
    COLneg <- lapply(x[-1], ncolon)
    x[-1] <- lapply(seq_along(x)[-1], function(i) {
        x[[i]][unlist(COLneg[i - 1])]
    })
    append2 <- function(x, y = ":", z) {
        lapply(z, function(z) {
            x <<- append(x, y, after = z)
        })
        x
    }
    x2 <- lapply(seq_along(x)[-1], function(n) append2(x[[n]], 
        z = COL[[n - 1]]))
    x3 <- lapply(x2, function(v) {
        data.frame(start = v[-c(f <- which(v == ":"), f + 1)], 
            end = v[-c(f, f - 1)], stringsAsFactors = FALSE)
    })
    names(x3) <- names(x)[-1]
    tonum <- function(z) {
        v <- apply(do.call(rbind, strsplit(z, "\\.")), 2, as.numeric)
        v[, 1] * 60^2 + v[, 2] * 60 + v[, 3]
    }
    x3 <- lapply(seq_along(x3), function(i) {
        data.frame(code = names(x3)[i], apply(x3[[i]], 2, tonum))
    })
    span <- tonum(x[[1]])
    print(paste0(paste0("start time = ", span[1]), paste0("; end time = ", 
        span[2])))
    do.call(rbind, x3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
