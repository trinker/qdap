\name{plot.polarity}
\alias{plot.polarity}
\title{Plots a polarity Object}
\usage{
  \method{plot}{polarity} (x, bar.size = 5, low = "red",
    mid = "grey99", high = "blue",
    ave.polarity.shape = "+", alpha = 1/4, shape = 19,
    point.size = 2.5, jitter = 0.1, nrow = NULL,
    na.rm = TRUE, ...)
}
\arguments{
  \item{x}{The polarity object.}

  \item{bar.size}{The size of the bars used in the Gantt
  plot.}

  \item{low}{The color to be used for lower values.}

  \item{mid}{The color to be used for mid-range values
  (default is a less striking color).}

  \item{high}{The color to be used for higher values.}

  \item{ave.polarity.shape}{The shape of the average
  polarity score used in the dot plot.}

  \item{alpha}{Transparency level of points (ranges between
  0 and 1).}

  \item{shape}{The shape of the points used in the dot
  plot.}

  \item{point.size}{The size of the points used in the dot
  plot.}

  \item{jitter}{Amount of vertical jitter to add to the
  points.}

  \item{nrow}{The number of rows in the dotplot legend
  (used when the number of grouping variables makes the
  legend too wide).  If NULL no legend if plotted.}

  \item{na.rm}{logical. Should missing values be removed?}

  \item{\ldots}{ignored}
}
\value{
  Invisibly returns the \code{ggplot2} objects that form
  the larger plot.
}
\description{
  Plots a polarity object as a heat map Gantt plot with
  polarity over time (measured in words) and polarity
  scores per sentence.  In the Gantt plot the black dots
  are the average polarity per grouping variable.
}

