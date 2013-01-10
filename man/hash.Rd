\name{hash}
\alias{hash}
\title{Hash/Dictionary Lookup}
\usage{
  hash(x)
}
\arguments{
  \item{x}{A two column dataframe}
}
\value{
  Creates a "hash table" or a two column data frame in its
  own evironment.
}
\description{
  Creates a new environemnt for quick hash style dictionary
  lookup.
}
\examples{
\dontrun{
new.hash <- hash(aggregate(conc~Plant, CO2, sum))
new.hash <- hash(aggregate(conc~Plant, CO2, sum))
sapply(as.character(CO2$Plant), function(x) {
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

