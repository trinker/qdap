\name{hash}
\alias{\%ha\%}
\alias{hash}
\alias{hash_look}
\title{Hash/Dictionary Lookup}
\usage{
hash(x, mode.out = "numeric")

hash_look(terms, envir, missing = NA)

terms \%ha\% envir
}
\arguments{
  \item{x}{A two column dataframe.}

  \item{mode.out}{The type of output (column 2) expected
  (e.g., \code{"character"}, \code{"numeric"}, etc.)}

  \item{terms}{A vector of terms to undergo a lookup.}

  \item{envir}{The hash environment to use.}

  \item{missing}{Value to assign to terms not found in the
  hash table.}
}
\value{
Creates a "hash table", a two column data frame in its own
environment.
}
\description{
\code{hash} - Creates a new environment for quick hash
style dictionary lookup.

\code{hash_look} - Works with a hash table such as is
returned from \code{hash}, to lookup values.

\code{\%ha\%} - A binary operator version of
\code{hash_look}.
}
\examples{
\dontrun{
(DF <- aggregate(mpg~as.character(carb), mtcars, mean))
new.hash1 <- hash(DF)  #numeric outcome
x <- sample(DF[, 1], 20, TRUE)
hash_look(x, new.hash1)

new.hash2 <- hash(DF, "character") #character outcome
x \%ha\% new.hash2
}
}
\author{
Bryan Goodrich and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
\url{http://www.talkstats.com/showthread.php/22754-Create-a-fast-dictionary}
}
\seealso{
\code{\link[qdap]{lookup}}, \code{\link[base]{environment}}
}
\keyword{dictionary,}
\keyword{hash,}
\keyword{lookup}

