\name{paste2}
\alias{paste2}
\title{Paste an Unspecified Number Of Text Columns}
\usage{
  paste2(multi.columns, sep = ".", handle.na = TRUE,
    trim = TRUE)
}
\arguments{
  \item{multi.columns}{The multiple columns or a list of
  vectors to paste together.}

  \item{sep}{A character string to separate the terms.}

  \item{handle.na}{logical.  If TRUE returns \code{NA} if
  any column/vector contains a missing value.}

  \item{trim}{logical.  If TRUE leading/trailing white
  space is removed.}
}
\value{
  Returns a vector with row-wise elements pasted together.
}
\description{
  Paste unspecified columns or a list of vectors together.
}
\note{
  \code{\link[base]{paste}} differs from
  \code{\link[qdap]{paste2}} because \code{paste} does not
  allowed an unspecified number of columns to be pasted.
  This behavior can be convenient for inside of functions
  when the number of columns being pasted is unknown.
}
\examples{
\dontrun{
v <- rep(list(state.abb[1:8],  month.abb[1:8]) , 5)
n <- sample(5:10, 1)
paste(v[1:n]) #odd looking return
paste2(v[1:n])
paste2(v[1:n], sep="|")
paste2(mtcars[1:10,], sep="|")
paste(mtcars[1:10,], sep="|") #odd looking return
paste2(CO2[1:10,], sep="|-|")
}
}
\seealso{
  \code{\link[base]{paste}}
}
\keyword{paste}

