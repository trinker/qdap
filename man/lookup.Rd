\name{lookup}
\alias{\%l+\%}
\alias{\%l\%}
\alias{lookup}
\title{Hash Table/Dictionary Lookup}
\usage{
lookup(terms, key.match, key.reassign = NULL, missing = NA)

terms \%l\% key.match

terms \%l+\% key.match
}
\arguments{
  \item{terms}{A vector of terms to undergo a lookup.}

  \item{key.match}{Takes one of the following: (1) a two
  column data.frame of a match key and reassignment column,
  (2) a named list of vectors (Note: if data.frame or named
  list supplied no key reassign needed) or (3) a single
  vector match key.}

  \item{key.reassign}{A single reassignment vector supplied
  if key.match is not a two column data.frame/named list.}

  \item{missing}{Value to assign to terms not matching the
  key.match.  If set to \code{NULL} the original values in
  \code{terms} corresponding to the missing elements are
  retained.}
}
\value{
Outputs A new vector with reassigned values.
}
\description{
\code{lookup} - Environment based hash table useful for
large vector lookups.

\code{\%l\%} - A binary operator version of \code{lookup}
for when \code{key.match} is a data.frame or named list.

\code{\%l+\%} - A binary operator version of \code{lookup}
for when \code{key.match} is a data.frame or named list and
\code{missing} is assumed to be \code{NULL}.
}
\examples{
\dontrun{
## Supply a dataframe to key.match

lookup(1:5, data.frame(1:4, 11:14))

## Retain original values for missing
lookup(1:5, data.frame(1:4, 11:14), missing=NULL)

lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))

key <- data.frame(x=1:2, y=c("A", "B"))
big.vec <- sample(1:2, 3000000, T)
out <- lookup(big.vec, key)
out[1:20]

## Supply a named list of vectors to key.match

codes <- list(
    A = c(1, 2, 4),
    B = c(3, 5),
    C = 7,
    D = c(6, 8:10)
)

lookup(1:10, codes)

## Supply a single vector to key.match and key.assign

lookup(mtcars$carb, sort(unique(mtcars$carb)),
    c('one', 'two', 'three', 'four', 'six', 'eight'))

lookup(mtcars$carb, sort(unique(mtcars$carb)),
    seq(10, 60, by=10))

## A big string to recode with variation
## means a bigger dictionary
recode_me <- sample(1:(length(LETTERS)*10), 1000000, TRUE)

## Time it
tic <- Sys.time()

output <- recode_me \%l\% split(1:(length(LETTERS)*10), LETTERS)
difftime(Sys.time(), tic)

## view it
sample(output, 100)


## \%l\%, a binary operator version of lookup
1:5 \%l\% data.frame(1:4, 11:14)
1:10 \%l\% codes

1:12 \%l\% codes
1:12 \%l+\% codes
}
}
\seealso{
\code{\link[base]{new.env}}
}
\keyword{dictionary,}
\keyword{hash,}
\keyword{lookup}

