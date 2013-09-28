\name{theme_qdap}
\alias{theme_qdap}
\title{ggplot2 Theme with No Background or Gridlines.}
\usage{
  theme_qdap(x = FALSE, y = FALSE)
}
\arguments{
  \item{x}{logical.  If \code{TRUE} vertical gridlines are
  added.}

  \item{y}{logical.  If \code{TRUE} horizontal gridlines
  are added.}
}
\description{
  A ggplot2 theme with no background and no gridlines.
}
\note{
  Both x and y can not be \code{TRUE}.  Use
  \code{\link[ggplot2]{theme_bw}} instead.
}
\examples{
ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap()
ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(x = TRUE)
ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(y = TRUE)
ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(x = TRUE, y = TRUE)
}
\seealso{
  \code{\link[ggplot2]{theme}} importFrom ggplot2 theme_bw
  theme element_blank
}

