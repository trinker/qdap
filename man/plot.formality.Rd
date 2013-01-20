\name{plot.formality}
\alias{plot.formality}
\title{Plots a formality Object}
\usage{
  \method{plot}{formality} (x, point.pch = 20,
    point.cex = 0.5, point.colors = c("gray65", "red"),
    bar.colors = NULL, short.names = FALSE,
    min.wrdcnt = NULL, ...)
}
\arguments{
  \item{x}{The formality object.}

  \item{point.pch}{The plotting symbol.}

  \item{point.cex}{The plotting symbol size.}

  \item{point.colors}{A vector of colors (length of two) to
  plot word count and formality score.}

  \item{bar.colors}{A palette of colors to supply to the
  bars in the visualization.  If two palettes are provided
  to the two bar plots respectively.}

  \item{short.names}{logical.  If TRUE shortens the length
  of legend and label names for more compact plot width.}

  \item{min.wrdcnt}{A minimum word count threshold that
  must be achieved to be considered in the results.
  Default includes all subgroups.}

  \item{\ldots}{ignored}
}
\value{
  Invisibly returns the \code{ggplot2} objects that form
  the larger plot.
}
\description{
  Plots a formality object including the parts of speech
  used to calculate contextual/formal speech.
}

