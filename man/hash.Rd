\name{hash}
\alias{hash}
\title{Hash/Dictionary Lookup}
\usage{
  hash(x, mode.out = "numeric")
}
\arguments{
  \item{x}{A two column dataframe.}

  \item{mode.out}{The type of output (column 2) expected
  (e.g. \code{"character"}, \code{"numeric"}, etc.)}
}
\value{
  Creates a "hash table" or a two column data frame in its
  own environment.
}
\description{
  Creates a new environemnt for quick hash style dictionary
  lookup.
}
\examples{
\dontrun{
(DF <- aggregate(mpg~as.character(carb), mtcars, mean))

new.hash <- hash(DF)
sapply(as.character(mtcars$carb), function(x) {
        if(exists(x, env = new.hash)) {
            get(x, e = new.hash)
        } else {
            NA
        }
    }
)

new.hash <- hash(DF, "character")
sapply(as.character(mtcars$carb), function(x) {
        if(exists(x, env = new.hash)) {
            get(x, e = new.hash)
        } else {
            NA
        }
    }
)
}
}
\author{
  Bryan Goodrich and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
  \url{http://www.talkstats.com/showthread.php/22754-Create-a-fast-dictionary}
}
\seealso{
  \code{\link[qdap]{lookup}},
  \code{\link[base]{environment}}
}
\keyword{dictionary,}
\keyword{hash,}
\keyword{lookup}

