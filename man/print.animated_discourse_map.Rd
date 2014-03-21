\name{print.animated_discourse_map}
\alias{print.animated_discourse_map}
\title{Prints a animated_discourse_map  Object}
\usage{
\method{print}{animated_discourse_map}(x, title = NULL,
  seed = sample(1:10000, 1), layout = layout.circle, ...)
}
\arguments{
  \item{x}{The animated_discourse_map object.}

  \item{title}{The title of the plot.}

  \item{seed}{The seed to use in plotting the graph.}

  \item{layout}{\pkg{igraph} \code{layout} to use.}

  \item{\ldots}{Other Arguments passed to
  \code{\link[igraph]{plot.igraph}}.}
}
\description{
Prints a animated_discourse_map object.
}

