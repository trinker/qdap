\name{formality}
\alias{formality}
\title{Formality Score}
\usage{
formality(text.var, grouping.var = NULL, order.by.formality = TRUE,
  digits = 2, ...)
}
\arguments{
  \item{text.var}{The text variable (or an object from
  \code{\link[qdap]{pos}}, \code{\link[qdap]{pos_by}} or
  \code{\link[qdap]{formality}}.  Passing the later three
  object will greatly reduce run time.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{order.by.formality}{logical.  If \code{TRUE} orders
  the results by formality score.}

  \item{digits}{The number of digits displayed.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{pos_by}}.}
}
\value{
A list containing at the following components:
\item{text}{The text variable} \item{POStagged}{Raw part of
speech for every word of the text variable}
\item{POSprop}{Part of speech proportion for every word of
the text variable} \item{POSfreq}{Part of speech count for
every word of the text variable} \item{pos.by.freq}{The
part of speech count for every word of the text variable by
grouping variable(s)} \item{pos.by.prop}{The part of speech
proportion for every word of the text variable by grouping
variable(s)} \item{form.freq.by}{The nine broad part of
speech categories count for every word of the text variable
by grouping variable(s)} \item{form.prop.by}{The nine broad
part of speech categories proportion for every word of the
text variable by grouping variable(s)}
\item{formality}{Formality scores by grouping variable(s)}
\item{pos.reshaped}{An expanded formality scores output
(grouping, word.count, pos & form.class) by word}
}
\description{
Transcript apply formality score by grouping variable(s)
and optionally plot the breakdown of the model.
}
\details{
Heylighen & Dewaele(2002)'s formality score is calculated
as: \deqn{F = 50(\frac{n_{f}-n_{c}}{N} + 1)}

Where: \deqn{f = \left \{noun, \;adjective, \;preposition,
\;article\right \}} \deqn{c = \left \{pronoun, \;verb,
\;adverb, \;interjection\right \}} \deqn{N = \sum{(f \;+
\;c \;+ \;conjunctions)}}
}
\section{Warning}{
  Heylighen & Dewaele (2002) state, "At present, a sample
  would probably need to contain a few hundred words for
  the measure to be minimally reliable. For single
  sentences, the F-value should only be computed for
  purposes of illustration" (p. 24).
}
\examples{
\dontrun{
with(DATA, formality(state, person))
(x1 <- with(DATA, formality(state, list(sex, adult))))
plot(x1)
plot(x1, short.names = FALSE)

scores(x1)
counts(x1)
proportions(x1)
preprocessed(x1)

plot(scores(x1))
plot(counts(x1))
plot(proportions(x1), high="darkgreen")
plot(preprocessed(x1))

data(rajPOS) #A data set consisting of a pos list object
x2 <- with(raj, formality(rajPOS, act))
plot(x2)
x3 <- with(raj, formality(rajPOS, person))
plot(x3, bar.colors="Dark2")
plot(x3, bar.colors=c("Dark2", "Set1"))
x4 <- with(raj, formality(rajPOS, list(person, act)))
plot(x4, bar.colors=c("Dark2", "Set1"))

rajDEM <- key_merge(raj, raj.demographics) #merge demographics with transcript.
x5 <- with(rajDEM, formality(rajPOS, sex))
plot(x5, bar.colors="RdBu")
x6 <- with(rajDEM, formality(rajPOS, list(fam.aff, sex)))
plot(x6, bar.colors="RdBu")
x7 <- with(rajDEM, formality(rajPOS, list(died, fam.aff)))
plot(x7, bar.colors="RdBu",  point.cex=2, point.pch = 3)
x8 <- with(rajDEM, formality(rajPOS, list(died, sex)))
plot(x8, bar.colors="RdBu",  point.cex=2, point.pch = "|")

names(x8)
colsplit2df(x8$formality)

#pass an object from pos or pos_by
ltruncdf(with(raj, formality(x8 , list(act, person))), 6, 4)
}
}
\references{
Heylighen, F., & Dewaele, J.M. (2002). Variation in the
contextuality of language: An empirical measure. Context in
Context, Special issue of Foundations of Science, 7 (3),
293-340.
}
\keyword{explicit,}
\keyword{formality,}
\keyword{parts-of-speech,}
\keyword{pos}

