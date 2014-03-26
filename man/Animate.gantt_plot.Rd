\name{Animate.gantt_plot}
\alias{Animate.gantt_plot}
\title{Gantt Plot}
\usage{
\method{Animate}{gantt_plot}(x, wc.time = TRUE, time.constant = 2, ...)
}
\arguments{
  \item{x}{The gantt_plot object.}

  \item{wc.time}{logical.  If \code{TRUE} weights duration
  of frame by word count.}

  \item{time.constant}{A constant to divide the maximum
  word count by.  Time is calculated by `round(exp(WORD
  COUNT/(max(WORD COUNT)/time.constant)))`. Therefore a
  larger constant will make the difference between the
  large and small word counts greater.}

  \item{\ldots}{ignored}
}
\description{
\code{gantt_plot} - Animate discourse from
\code{\link[qdap]{gantt_wrap}},
\code{\link[qdap]{gantt_wrap}}, or any other Gantt plotting
method.
}
\details{
gantt_plot Method for Animate
}

