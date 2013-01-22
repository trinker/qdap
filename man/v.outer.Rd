\name{v.outer}
\alias{v.outer}
\title{Vectorized Version of outer}
\usage{
  v.outer(x, FUN, ...)
}
\arguments{
  \item{x}{A \code{matrix}, \code{dataframe} or equal
  length \code{list} of vectors.}

  \item{FUN}{A vectorized function.}

  \item{\ldots}{Other arguments passed to the function
  supplied to \code{FUN}.}
}
\value{
  Returns a matrix with the vectorized
  \code{\link[base]{outer}} function.
}
\description{
  Vectorized \code{\link[base]{outer}}.
}
\examples{
\dontrun{
pooled.sd <- function(x, y) {
    n1 <- length(x)
    n2 <- length(y)
    s1 <- sd(x)
    s2 <- sd(y)
    sqrt(((n1-1)*s1 + (n2-1)*s2)/((n1-1) + (n2-1)))
}

euc.dist <- function(x,y) sqrt(sum((x - y) ^ 2))
sum2 <- function(x, y) sum(x, y)

v.outer(mtcars, cor)
v.outer(mtcars, pooled.sd)
v.outer(mtcars, euc.dist)
v.outer(mtcars, sum2)


mtcars2 <- lapply(mtcars, "[")
v.outer(mtcars2, cor)
v.outer(mtcars2, cor,  method = "spearman")
v.outer(mtcars2, pooled.sd)
print(v.outer(mtcars, pooled.sd), digits = 1)
print(v.outer(mtcars, pooled.sd), digits = NULL)
v.outer(mtcars2, euc.dist)
v.outer(mtcars2, sum2)

wc3 <- function(x, y) sum(sapply(list(x, y), wc, byrow = FALSE))
(L1 <- word_list(DATA$state, DATA$person)$cwl)
(x <- v.outer(L1, wc3))
diag(x) <- (sapply(L1, length))
x
}
}
\seealso{
  \code{\link[base]{outer}}, \code{\link[stats]{cor}}
}

