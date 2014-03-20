\name{discourse_map}
\alias{discourse_map}
\title{Discourse Mapping}
\usage{
discourse_map(text.var, grouping.var, edge.constant, sep = "_", ...)
}
\arguments{
  \item{text.var}{The text variable or a
  \code{"word_stats"} object (i.e., the output of a
  \code{word_stats} function).}

  \item{grouping.var}{The grouping variables.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{edge.constant}{A constant to multiple the edges by.
  Defaults (if \code{missing}) to 2.5 times the number of
  social actors.}

  \item{\ldots}{ignored}

  \item{sep}{The separator character to use between
  grouping variables.}
}
\value{
Returns a list: \item{raw}{The dataframe with to and from
columns (the edges) + word counts} \item{edge_word_count}{A
dataframe of edges and word counts + proportional word
count} \item{vertex_word_count}{A dataframe of vertices and
word counts + proportional word count} \item{plot}{An
\pkg{igraph} object}
}
\description{
View the flow of discourse from social actors.
}
\examples{
\dontrun{
discourse_map(DATA$state, list(DATA$person, DATA$sex))
x <- with(mraja1, discourse_map(dialogue, person))
x
lview(x)
library(igraph)
plot(visual(x), edge.curved=FALSE)

## Augmenting the plot
mygraph <- visual(x)

plot(mygraph, edge.curved=TRUE)

V(mygraph)$sex <- V(mygraph)$name \%l\% raj.demographics[, 1:2]
V(mygraph)$color <- ifelse(V(mygraph)$sex=="f", "pink", "lightblue")

plot(mygraph, edge.curved=TRUE)

V(mygraph)$family <- V(mygraph)$name \%l+\% raj.demographics[, c(1, 3)]
cols <- qcv(blue, red, brown, darkgreen, grey10)
V(mygraph)$label.color <- lookup(V(mygraph)$family,
    unique(V(mygraph)$family), cols)

plot(mygraph, edge.curved=TRUE)

## Repeated Measures (BASIC EXAMPLE)
##------------------------------

## First merge data and map to discourse per act
## to separate networks

dat <- key_merge(raj, raj.demographics)
list_dat <- split(dat, dat$act)
plot_dat <- lapply(list_dat, function(x) with(x, discourse_map(dialogue, person)))

opar <- par()$mar
par(mfrow=c(3, 2), mar=c(0, 0, 3, 0))

lapply(seq_along(plot_dat), function(i){
    plot(plot_dat[[i]])
    mtext(paste("Act", names(plot_dat)[i]), side=3)
})


## Repeated Measures (EXTENDED EXAMPLE)
##------------------------------

fam_key <- data.frame(unique(raj.demographics$fam.aff),
    cols = qcv(blue, , grey10, red, brown))

par(mfrow=c(3, 2), mar=c(0, 1, 3, 1))
lapply(seq_along(plot_dat), function(i){

    THE_PLOT <- visual(plot_dat[[i]])

    V(THE_PLOT)$sex <- V(THE_PLOT)$name \%l\% raj.demographics[, 1:2]
    V(THE_PLOT)$color <- ifelse(V(THE_PLOT)$sex=="f", "pink", "lightblue")
    V(THE_PLOT)$family <- V(THE_PLOT)$name \%l+\% raj.demographics[, c(1, 3)]
    V(THE_PLOT)$label.color <- lookup(V(THE_PLOT)$family, fam_key)

    plot(THE_PLOT, edge.curved=TRUE)
    mtext(paste("Act", names(plot_dat)[i]), side=3)
})

## Reset graphics margins
par(mar=opar)
}
}

