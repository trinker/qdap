\name{plot.wfm}
\alias{plot.wfm}
\title{Plots a wfm object}
\usage{
  \method{plot}{wfm} (x, non.zero = FALSE, digits = 0,
    by.column, high = ifelse(non.zero, "black", "blue"),
    grid = ifelse(non.zero, "black", "white"), ...)
}
\arguments{
  \item{x}{The wfm object}

  \item{non.zero}{logical.  If \code{TRUE} all values
  coverted to dummy coded based on x_ij > 0.}

  \item{digits}{The number of digits displayed if
  \code{values} is \code{TRUE}.}

  \item{by.column}{logical.  If \code{TRUE} applies scaling
  to the column.  If \code{FALSE} applies scaling by row
  (use \code{NULL} to turn off scaling).}

  \item{high}{The color to be used for higher values.}

  \item{grid}{The color of the grid (Use \code{NULL} to
  remove the grid).}

  \item{\ldots}{Other arguments passed to qheat.}
}
\description{
  Plots a wfm object.
}

