\name{plot.sums_gantt}
\alias{plot.sums_gantt}
\title{Plots a sums_gantt object}
\usage{
  \method{plot}{sums_gantt} (x, base = TRUE, title = NULL,
    ...)
}
\arguments{
  \item{x}{The sums_gantt object}

  \item{base}{logical.  If \code{TRUE} prints in base
  graphics system. If \code{FALSE} prints in ggplot
  graphics system.}

  \item{plot}{logical.  If \code{TRUE} the plot will
  automatically plot. The user may wish to set to
  \code{FALSE} for use in knitr, sweave, etc. to add
  additional plot layers.}

  \item{title}{An optional title.}

  \item{\ldots}{Other arguments passed to
  \code{sums_gantt_wrap} or \code{plot_gantt_base}}
}
\description{
  Plots a sums_gantt object.
}

