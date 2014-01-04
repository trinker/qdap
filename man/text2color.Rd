\name{text2color}
\alias{text2color}
\title{Map Words to Colors}
\usage{
text2color(words, recode.words, colors)
}
\arguments{
  \item{words}{A vector of words.}

  \item{recode.words}{A vector of unique words or a list of
  unique word vectors that will be matched against
  corresponding colors.}

  \item{colors}{A vector of colors of equal in length to
  recode.words + 1(the +1 is for unmatched words).}
}
\value{
Returns a vector of mapped colors equal in length to the
words vector.
}
\description{
A dictionary lookup that maps words to colors.
}
\examples{
\dontrun{
set.seed(10)
x <- data.frame(X1 = sample(Top25Words[1:10], 20, TRUE))

#blue was recycled
text2color(x$X1, qcv(the, and, is), qcv(red, green, blue))
text2color(x$X1, qcv(the, and, is), qcv(red, green, blue, white))
x$X2 <- text2color(x$X1, list(qcv(the, and, is), "that"),
    qcv(red, green, white))
x
}
}
\seealso{
\code{\link[qdap]{lookup}}
}
\keyword{color,}
\keyword{dictionary}
\keyword{lookup,}
\keyword{recode,}

