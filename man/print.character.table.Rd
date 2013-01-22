\name{print.character.table}
\alias{print.character.table}
\title{Prints a character.table object}
\usage{
  \method{print}{character.table} (x, digits = 2,
    percent = NULL, zero.replace = NULL, ...)
}
\arguments{
  \item{x}{The character.table object}

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
  Prints a character.table object.
}

