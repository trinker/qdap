\name{plot.gantt}
\alias{plot.gantt}
\title{Plots a gantt object}
\usage{
  \method{plot}{gantt} (x, base = FALSE, title = NULL,
    plot = TRUE, ...)
}
\arguments{
  \item{x}{The sums_gantt object}

  \item{base}{logical.  If \code{TRUE} prints in base
  graphics system. If \code{FALSE} prints in ggplot
  graphics system.}

  \item{title}{An optional title.}

  \item{plot}{logical.  If \code{TRUE} the plot will
  automatically plot. The user may wish to set to
  \code{FALSE} for use in knitr, sweave, etc. to add
  additional plot layers.}

  \item{\ldots}{Other arguments passed to
  \code{sums_gantt_wrap} or \code{plot_gantt_base}}
}
\description{
  Plots a gantt object.
}

