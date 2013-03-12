\name{word.network.plot}
\alias{word.network.plot}
\title{Word Network Plot}
\usage{
  word.network.plot(text.var, grouping.var = NULL,
    target.words = NULL, stopwords = Top100Words,
    label.cex = 0.8, label.size = 0.5, edge.curved = TRUE,
    vertex.shape = "circle", edge.color = "gray70",
    label.colors = "black", layout = NULL,
    title.name = NULL, title.padj = -4.5,
    title.location = 3, title.font = NULL, title.cex = 0.8,
    log.labels = FALSE, title.color = "black",
    legend = NULL, legend.cex = 0.8,
    legend.location = c(-1.54, 1.41), plot = TRUE,
    char2space = "~~", ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{target.words}{A named list of vectors of words
  whose length corresponds to \code{label.colors} (+1
  length in cloud colors for non-matched terms).}

  \item{stopwords}{Words to exclude from the analysis
  (default is Top100Words).}

  \item{label.cex}{The magnification to be used for network
  plot labels relative to the current setting of cex.
  Default is .8.}

  \item{log.labels}{logical.  If TRUE uses a proportional
  log label for more readable labels.  The formula is:
  \code{log(SUMS)/max(log(SUMS)))}. \code{label.size} adds
  more control over the label sizes.}

  \item{label.size}{An optional sizing constant to add to
  labels if log.labels is TRUE.}

  \item{edge.curved}{logical.  If TRUE edges will be curved
  rather than straight paths.}

  \item{vertex.shape}{The shape of the vertices (see
  \code{\link[igraph]{igraph.vertex.shapes}} for more).}

  \item{edge.color}{A character vector of length one
  corresponding to the color of the plot edges.}

  \item{label.colors}{A character vector of length one
  corresponding to the color of the labels.}

  \item{layout}{Layout types supported by igraph.  See
  \code{\link[igraph]{layout}}.}

  \item{title.name}{The title of the plot.}

  \item{title.padj}{Adjustment for the network plot title.
  For strings parallel to the axes, padj = 0 means right or
  top alignment, and padj = 1 means left or bottom
  alignment.}

  \item{title.location}{On which side of the network plot
  (1=bottom, 2=left, 3=top, 4=right).}

  \item{title.font}{The font family of the cloud title.}

  \item{title.cex}{Character expansion factor for the
  title. NULL and NA are equivalent to 1.0.}

  \item{title.color}{A character vector of length one
  corresponding to the color of the title.}

  \item{legend}{A character vector of names corresponding
  to the number of vectors in \code{match.string}.}

  \item{legend.cex}{Character expansion factor for the
  network plot legend. NULL and NA are equivalent to 1.0.}

  \item{legend.location}{The x and y co-ordinates to be
  used to position the network plot legend.  The location
  may also be specified by setting x to a single keyword
  from the list \code{"bottomright"}, \code{"bottom"},
  \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
  \code{"top"}, \code{"topright"}, \code{"right"} and
  \code{"center"}. This places the legend on the inside of
  the plot frame at the given location.}

  \item{plot}{logical.  If TRUE plots a network plot of the
  words.}

  \item{char2space}{A vector of characters to be turned
  into spaces.  If \code{char.keep} is NULL,
  \code{char2space} will activate this argument.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{strip}}.}
}
\description{
  A network plot of words.  Shows the interconnected and
  supporting use of words between textual units containing
  key terms.
}
\note{
  Words can be kept as one by inserting a double tilde
  (\code{"~~"}), or other character strings passed to
  char2space, as a single word/entry. This is useful for
  keeping proper names as a single unit.
}
\examples{
\dontrun{
word.network.plot(text.var=DATA$state, grouping.var=DATA$person)
word.network.plot(text.var=DATA$state, grouping.var=list(DATA$sex,
    DATA$adult))
word.network.plot(text.var=DATA$state, grouping.var=DATA$person,
    title.name = "TITLE", log.labels=TRUE)
word.network.plot(text.var=raj.act.1$dialogue, grouping.var=raj.act.1$person,
  stopwords = Top200Words)

#inset double tilde ("~~") to keep dual words (e.i. first last name)
alts <- c(" fun", "I ")
state2 <- mgsub(alts, gsub("\\\\s", "~~", alts), DATA$state)
word.network.plot(text.var=state2, grouping.var=DATA$person)
}
}
\seealso{
  \code{\link[qdap]{word.network.plot}},
  \code{\link[igraph]{graph.adjacency}}
}
\keyword{network}

