\name{cm_time.temp}
\alias{cm_time.temp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time Span Code Sheet
}
\description{
Generates a time span coding sheet and coding format sheet.
}
\usage{
cm_time.temp(codes, start = ":00", end, file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{codes}{
%%     ~~Describe \code{codes} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
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
function (codes, start = ":00", end, file = NULL) 
{
    x1 <- matrix(c("list(", paste0("    ", codes[1:(length(codes) - 
        1)], " = c(),"), paste0("    ", codes[length(codes)], 
        " = c()"), ")"), ncol = 1)
    st <- unlist(strsplit(start, ":"))
    en <- as.numeric(unlist(strsplit(end, ":")))
    st[1] <- ifelse(st[1] == "", "0", st[1])
    st <- as.numeric(st)
    x <- (en[1] - st[1]) + 1
    z <- matrix(rep(0:59, x), nrow = x, byrow = TRUE)
    rownames(z) <- c(paste0("[", st[1]:en[1], "]"))
    colnames(z) <- rep("", ncol(z))
    if (st[2] > 0) {
        z[1, 0:(st[2])] <- NA
    }
    if (en[2] < 59) {
        z[x, (en[2] + 2):60] <- NA
    }
    zz <- matrix(capture.output(print(z, na.print = ""))[-1], 
        ncol = 1)
    print(z, na.print = "")
    cat("\n\n")
    cat(paste("list(\n", paste0("    ", paste0("\b", paste(codes, 
        collapse = " = c(),\n    "), " = c()")), "\n)\n"))
    dimnames(zz) <- list(c(rep("", x)), c(""))
    if (Sys.info()["sysname"] == "Windows") {
        writeClipboard(noquote(rbind(zz, "", "", x1)), format = 1)
    }
    if (Sys.info()["sysname"] == "Darwin") {
        j <- pipe("pbcopy", "w")
        writeLines(noquote(rbind(zz, "", "", x1)), con = j)
        close(j)
    }
    if (!is.null(file)) {
        v <- paste0(zz, "\n")
        cat(v[1], file = file)
        lapply(2:x, function(i) cat(v[i], file = file, append = TRUE))
        cat(paste("list(\n", paste0("    ", paste0("\b", paste(codes, 
            collapse = " = c(),\n    "), " = c()")), "\n)\n"), 
            file = file, append = TRUE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
