\name{print.pos.by}
\alias{print.pos.by}
\title{Prints a pos.by Object.}
\usage{
  \method{print}{pos.by} (x, digits = 1, percent = NULL,
    zero.replace = NULL, ...)
}
\arguments{
  \item{x}{The pos.by object}

  \item{digits}{Integer values specifying the number of
  digits to be printed.}

  \item{percent}{logical.  If TRUE output given as percent.
  If FALSE the output is proportion.  If NULL uses the
  value from \code{\link[qdap]{termco}}.  Only used if
  \code{label} is TRUE.}

  \item{zero.replace}{Value to replace 0 values with.  If
  NULL uses the value from \code{\link[qdap]{termco}}.
  Only used if \code{label} is TRUE.}

  \item{\ldots}{ignored}
}
\description{
  Prints a pos.by object.
}

