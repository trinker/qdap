\name{plot.word_proximity}
\alias{plot.word_proximity}
\title{Plots a word_proximity object}
\usage{
\method{plot}{word_proximity}(x, label = TRUE, lab.digits = 3,
  high = "blue", low = "white", grid = NULL, ...)
}
\arguments{
  \item{x}{The word_proximity object}

  \item{label}{logical.  If \code{TRUE} the cells of the
  heat map plot will be labeled with count and proportional
  values.}

  \item{lab.digits}{Integer values specifying the number of
  digits to be printed if \code{label} is \code{TRUE}.}

  \item{\ldots}{Other arguments passed to qheat.}
}
\description{
Plots a word_proximity object.
}

