\name{plot.word_cor}
\alias{plot.word_cor}
\title{Plots a word_cor object}
\usage{
\method{plot}{word_cor}(x, label = TRUE, lab.digits = 3, high = "red",
  low = "white", grid = NULL, ...)
}
\arguments{
  \item{x}{The word_cor object}

  \item{label}{logical.  If \code{TRUE} the cells of the
  heat map plot will be labeled with count and proportional
  values.}

  \item{lab.digits}{Integer values specifying the number of
  digits to be printed if \code{label} is \code{TRUE}.}

  \item{low}{The color to be used for lower values.}

  \item{high}{The color to be used for higher values.}

  \item{grid}{The color of the grid (Use \code{NULL} to
  remove the grid).}

  \item{\ldots}{Other arguments passed to qheat if matrix
  and other arguments passed to
  \code{\link[ggplot2]{geom_point}} if a list.}
}
\description{
Plots a word_cor object.
}

