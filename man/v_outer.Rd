\name{v_outer}
\alias{v_outer}
\title{Vectorized Version of outer}
\usage{
v_outer(x, FUN, ...)
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

v_outer(mtcars, cor)
v_outer(mtcars, pooled.sd)
v_outer(mtcars[, 1:7], euc.dist)
v_outer(mtcars[, 1:7], sum2)

#mtcars as a list
mtcars2 <- lapply(mtcars[, 1:7], "[")
v_outer(mtcars2, cor)
v_outer(mtcars2, cor,  method = "spearman")
v_outer(mtcars2, pooled.sd)
print(v_outer(mtcars[, 1:7], pooled.sd), digits = 1)
print(v_outer(mtcars[, 1:7], pooled.sd), digits = NULL)
v_outer(mtcars2, euc.dist)
v_outer(mtcars2, sum2)

wc3 <- function(x, y) sum(sapply(list(x, y), wc, byrow = FALSE))
L1 <- word_list(DATA$state, DATA$person)$cwl
(x <- v_outer(L1, wc3))
diag(x) <- (sapply(L1, length))
x

## Cosine similarity
cos_sim <- function(x, y) x \%*\% y / sqrt(x\%*\%x * y\%*\%y)
mat <- matrix(rbinom(500, 0:1, .45), ncol=10)
v_outer(mat, cos_sim)

v_outer(with(DATA, wfm(state, person)), cos_sim)
with(DATA, dissimilarity(state, person))
}
}
\author{
Vincent Zoonekynd and Tyler Rinker
<tyler.rinker@gmail.com>.
}
\seealso{
\code{\link[base]{outer}}, \code{\link[stats]{cor}}
}

