\name{outlier.labeler}
\alias{outlier.labeler}
\title{Locate Outliers in Numeric String}
\usage{
  outlier.labeler(x, standardize = TRUE, ...)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{standardize}{logical.  If TRUE scales the vector
  first.}

  \item{\ldots}{Other arguments passed to
  \code{\link[base]{scale}}.}
}
\value{
  Returns a matrix (one column) of possible outliers coded
  as \code{"3sd"}, \code{"2sd"} and \code{"1.5sd"},
  corrsponding to >= to 3, 2, or 1.5 standard deviations.
}
\description{
  Locate and label possible outliers in a string.
}
\examples{
\dontrun{
outlier.labeler(mtcars$hp)
by(mtcars$mpg, mtcars$cyl, outlier.labeler)
tapply(mtcars$mpg, mtcars$cyl, outlier.labeler)
}
}
\seealso{
  \code{\link[base]{scale}}
}
\keyword{scale,}
\keyword{standardize}

