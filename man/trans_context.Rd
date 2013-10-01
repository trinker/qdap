\name{trans_context}
\alias{trans_context}
\title{Print Context Around Indices}
\usage{
  trans_context(text.var, grouping.var, inds, n.before = 3,
    tot = TRUE, n.after = n.before)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{inds}{A list of integer indices to print context
  for.}

  \item{n.before}{The number of rows before the indexed
  occurence.}

  \item{tot}{logical.  If \code{TRUE} condenses sub-units
  (e.g., sentences) into turns of talk for that
  \code{grouping.var}.}

  \item{n.after}{The number of rows after the indexed
  occurence.}
}
\value{
  Returns a dataframe of the class "qdap_context" that can
  be printed (i.e., saved) in flexible outputs.  The
  dataframe can be printed as a dataframe style or pretty
  text output.  The resulting file contains n rows before
  and after each index of a vector of indices.
}
\description{
  Print (or save to an external file) n text elements
  before and after indices.
}
\examples{
\dontrun{
(x <- with(DATA, trans_context(state, person, inds=c(1, 4, 7, 11))))
print(x, pretty=FALSE)
print(x, double_space = FALSE)
print(x, file="foo.xlsx")
print(x, file="foo.csv")
print(x, file="foo.txt")
print(x, file="foo.txt", pretty = FALSE)
print(x, file="foo.doc")

## With `end_mark`
inds1 <- which(end_mark(DATA.SPLIT[, "state"]) == "?")
with(DATA.SPLIT, trans_context(state, person, inds=inds1))
with(DATA.SPLIT, trans_context(state, person, n.before = 0, inds=inds1))

## With `boolean_search`
inds2 <- boolean_search(DATA.SPLIT$state, " I &&.")
with(DATA.SPLIT, trans_context(state, person, inds=inds2))

inds3 <- boolean_search(DATA$state, " I ||.")
with(DATA.SPLIT, trans_context(state, person, inds=inds3))
with(DATA.SPLIT, trans_context(state, list(person, sex), inds=inds3))
with(DATA.SPLIT, trans_context(state, list(sex, adult), inds=inds3))

## With `question_type`
(x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person))
y <- x[["raw"]]
inds4 <- y[y[, "q.type"] \%in\% qcv(what, how), "n.row"]
with(DATA.SPLIT, trans_context(state, person, inds=inds4))
with(DATA.SPLIT, trans_context(state, person, inds=inds4, tot=F))
}
}
\seealso{
  \code{\link[qdap]{boolean_search}},
  \code{\link[qdap]{question_type}},
  \code{\link[qdap]{end_mark}}
}

