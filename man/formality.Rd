\name{formality}
\alias{formality}
\title{Formality Score}
\usage{
  formality(text.var, grouping.var = NULL, plot = FALSE,
    sort.by.formality = TRUE, digits = 2, point.pch = 20,
    point.cex = 0.5, point.colors = c("gray65", "red"),
    bar.colors = NULL, min.wrdcnt = NULL, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates formality score for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{plot}{logical. Provides a visualization for the
  results}

  \item{sort.by.formality}{logical.  If TRUE orders the
  results by formality score.}

  \item{digits}{The number of digits displayed.}

  \item{point.pch}{The plotting symbol.}

  \item{point.cex}{The plotting symbol size.}

  \item{point.colors}{A vector of colors (length of two) to
  plot word count and formality score.}

  \item{bar.colors}{A palette of colors to supply to the
  bars in the visualization.  If two palettes are provided
  to the two bar plots respectively.}

  \item{min.wrdcnt}{A minimum word count threshold that
  must be achieved to be considered in the results.
  Default includes all subgroups.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{pos.by}}.}
}
\value{
  A list containing at the following components:
  \item{text}{The text variable} \item{POStagged}{Raw part
  of speech for every word of the text variable}
  \item{POSprop}{Part of speech proportion for every word
  of the text variable} \item{POSfreq}{Part of speech count
  for every word of the text variable}
  \item{pos.by.freq}{The part of speech count for every
  word of the text variable by grouping variable(s)}
  \item{pos.by.prop}{The part of speech proportion for
  every word of the text variable by grouping variable(s)}
  \item{form.freq.by}{The nine broad part of speech
  categories count for every word of the text variable by
  grouping variable(s)} \item{form.prop.by}{The nine broad
  part of speech categories proportion for every word of
  the text variable by grouping variable(s)}
  \item{formality}{Formality scores by grouping
  variable(s)} \item{pos.reshaped}{An expanded formality
  scores output (grouping, word.count, pos & form.class) by
  word}
}
\description{
  Transcript apply formality score by grouping variable(s).
}
\details{
  Heylighen & Dewaele(2002)'s formality score is calculated
  as: \deqn{F = 50(\frac{n_{f}-n_{c}}{N} + 1)}

  Where: \deqn{f = \left \{noun, \;adjective,
  \;preposition, \;article\right \}} \deqn{c = \left
  \{pronoun, \;verb, \;adverb, \;interjection\right \}}
  \deqn{N = \sum{(f \;+ \;c \;+ \;conjunctions)}}
}
\note{
  Heylighen & Dewaele(2002) say "At present, a sample would
  probably need to contain a few hundred words for the
  measure to be minimally reliable. For single sentences,
  the F-value should only be computed for purposes of
  illustration".
}
\examples{
\dontrun{
with(DATA, formality(state, person))
with(DATA, formality(state, list(sex, adult), plot = TRUE))
rajDEM <- key_merge(raj, raj.demographics, 'person')
with(raj, formality(rajPOS, act, plot=TRUE))
with(raj, formality(rajPOS, person, plot=TRUE, bar.colors="Dark2"))
with(raj, formality(rajPOS, person, plot=TRUE, bar.colors=c("Dark2", "Set1")))
with(raj, formality(rajPOS, list(person, act), plot=TRUE, bar.colors="Set1"))
with(rajDEM, formality(rajPOS, sex, plot=TRUE, bar.colors="RdBu"))
with(rajDEM, formality(rajPOS, list(fam.aff, sex), plot=TRUE,
    bar.colors="RdBu"))
with(rajDEM, formality(rajPOS, list(died, fam.aff), plot=TRUE,
    bar.colors="RdBu",  point.cex=2, point.pch = 3))
raj.form <- with(rajDEM, formality(rajPOS, list(died, sex), plot=TRUE,
    bar.colors="RdBu",  point.cex=2, point.pch = "|"))
names(raj.form)
colsplit2df(raj.form$formality)
}
}
\references{
  Heylighen, F., & Dewaele, J.M. (2002). Variation in the
  contextuality of language: An empirical measure. Context
  in Context, Special issue of Foundations of Science, 7
  (3), 293-340.
}
\keyword{explicit}
\keyword{formality,}

