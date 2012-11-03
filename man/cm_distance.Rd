\name{cm_distance}
\alias{cm_distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cm_distance(dataframe, time.var = NULL, code.var = "code", causal = FALSE, start.var = "start", end.var = "end", mean.digits = 3, sd.digits = 3, safety = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{time.var}{
%%     ~~Describe \code{time.var} here~~
}
  \item{code.var}{
%%     ~~Describe \code{code.var} here~~
}
  \item{causal}{
%%     ~~Describe \code{causal} here~~
}
  \item{start.var}{
%%     ~~Describe \code{start.var} here~~
}
  \item{end.var}{
%%     ~~Describe \code{end.var} here~~
}
  \item{mean.digits}{
%%     ~~Describe \code{mean.digits} here~~
}
  \item{sd.digits}{
%%     ~~Describe \code{sd.digits} here~~
}
  \item{safety}{
%%     ~~Describe \code{safety} here~~
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
function (dataframe, time.var = NULL, code.var = "code", causal = FALSE, 
    start.var = "start", end.var = "end", mean.digits = 3, sd.digits = 3, 
    safety = TRUE) 
{
    if (!is.null(time.var)) {
        L1 <- split(dataframe, dataframe[, time.var])
    }
    else {
        L1 <- dataframe
    }
    L2 <- lapply(L1, function(x) split(x, x[, code.var]))
    NMS <- lapply(L2, names)
    NMS <- unlist(lapply(seq_along(NMS), function(i) {
        paste(names(NMS)[i], NMS[[i]], sep = ".")
    }))
    lens <- sapply(L2, length)
    mlens <- sapply(L2, function(x) {
        max(do.call(rbind, x)[, end.var])
    })
    nt <- rep(mlens, lens)
    v <- unlist(L2, recursive = FALSE)
    L3 <- lapply(seq_along(v), function(i) {
        cm_se2vect(v[[i]][, start.var], v[[i]][, end.var])
    })
    L4 <- lapply(seq_along(L3), function(i) {
        c(L3[[i]], rep(0, nt[i] - length(L3[[i]])))
    })
    dat <- do.call(cbind, L4)
    colnames(dat) <- NMS
    INDS <- cumsum(lens)
    INDS2 <- c(1, INDS + 1)
    INDS2 <- INDS2[-length(INDS2)]
    L5 <- lapply(seq_along(INDS), function(i) dat[, INDS[i]:INDS2[i]])
    names(L5) <- names(L2)
    L5 <- lapply(L5, function(x) {
        colnames(x) <- unlist(strsplit(colnames(x), "\\."))[c(F, 
            T)]
        return(x)
    })
    if (causal) {
        inds <- 4
    }
    else {
        inds <- 2
    }
    dism <- function(x, y) {
        cm_bidist(x, y)[[inds]][1]
    }
    dissd <- function(x, y) {
        cm_bidist(x, y)[[inds]][2]
    }
    disn <- function(x, y) {
        cm_bidist(x, y)[[inds]][3]
    }
    FUN <- function(dat, mdigs = mean.digits, sdigs = sd.digits) {
        dat <- data.frame(dat)
        means <- v.outer(dat, dism, digits = mdigs)
        sds <- v.outer(dat, dissd, digits = sdigs)
        ns <- v.outer(dat, disn, digits = 0)
        DIM <- dim(means)
        pfun <- function(x, y, z) paste0(x, "(", y, ")", "n=", 
            z)
        comb <- mgsub(c("(NA)", "NA;n=0"), c(";", NA), mapply(pfun, 
            means, sds, ns), fixed = TRUE)
        dim(comb) <- DIM
        dimnames(comb) <- list(rownames(means), colnames(means))
        stand <- means/sds
        stand[is.nan(stand)] <- NA
        list(mean = means, sd = sds, n = ns, combined = noquote(comb), 
            standardized = stand)
    }
    lapply(L5, FUN)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
