\name{kullback.leibler}
\alias{kullback.leibler}
\title{Kullback Leibler Statistic}
\usage{
  kullback.leibler(x, y = NULL, digits = 3)
}
\arguments{
  \item{x}{A numeric vector, matrix or data frame.}

  \item{y}{A second numeric vector if x is also a vector.
  Default is NULL.}

  \item{digits}{Number of decimal places to round.}
}
\value{
  Returns a matrix of the Kullback Leibler measure between
  each vector of probabilities.
}
\description{
  A proximity measure between two probability distributions
  applied to speech.
}
\details{
  Uses Kullback & Leibler's (1951) formula:
  \deqn{D_{KL}(P||Q)=\sum_i{ln\left ( \frac{P_{i}}{Q_{i}}
  \right )}P_{i}}
}
\note{
  The \code{kullback.leibler} function generally receives
  the output of either \code{wfm} or \code{wfdf} functions.
}
\examples{
\dontrun{
p.df <- wfdf(DATA$state, DATA$person)
p.mat <- wfm(text.var = DATA$state, grouping.var = DATA$person)

kullback.leibler(p.mat)
kullback.leibler(p.df)
kullback.leibler(p.df$greg, p.df$sam)

p.df2 <- wfdf(raj$dialogue, raj$person)
kullback.leibler(p.df2)
}
}
\references{
  Kullback, S., & Leibler, R.A. (1951). On Information and
  sufficiency. Annals of Mathematical Statistics 22 (1):
  79-86. doi:10.1214/aoms/1177729694
}
\keyword{Kullback-Leibler}

