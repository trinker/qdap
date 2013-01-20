\name{polarity}
\alias{polarity}
\title{Polarity Score (Sentiment Analysis)}
\usage{
  polarity(text.var, grouping.var = NULL,
    positive.list = positive.words,
    negative.list = negative.words,
    negation.list = negation.words,
    amplification.list = increase.amplification.words,
    rm.incomplete = FALSE, digits = 3, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{positive.list}{A character vector of terms
  indicating positive reaction.}

  \item{negative.list}{A character vector of terms
  indicating negative reaction.}

  \item{negation.list}{A character vector of terms
  reversing the intent of a positive or negative word.}

  \item{amplification.list}{A character vector of terms
  that increases the intensity of a positive or negative
  word.}

  \item{rm.incomplete}{logical.  If TRUE text rows ending
  with qdap's incomplete sentence end mark (\code{|}) will
  be removed from the analysis.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}

  \item{\ldots}{Other arguments supplied to \code{endf}.}
}
\value{
  Returns a list of two dataframes: \item{all}{A dataframe
  of scores per row with: \itemize{ \item wc - word count
  \item polarity - sentence polarity score \item raw - raw
  polarity score (considering only positive and negative
  words) \item negation.adj.raw - raw adjusted for negation
  words \item amplification.adj.raw - raw adjusted for
  amplification words \item pos.words - words considered
  positive \item neg.words - words considered negative} }
  \item{group}{A dataframe with the average polarity score
  by grouping variable.}
}
\description{
  Approximate the sentiment (polarity) of text by grouping
  variable(s).
}
\details{
  The equation used by the algorithm to assign value to
  polarity to each sentence fist utilizes the sentiment
  dictionary (Hu and Liu, 2004) to tag each word as either
  positive (\eqn{x_i^{+}}), negative (\eqn{x_i^{-}}),
  neutral (\eqn{x_i^{0}}), negator(\eqn{x_i\neg}), or
  amplifier (\eqn{x_i^{\uparrow}}).  Neutral words hold no
  value in the equation but do affect word count (\eqn{n}).
  Each positive (\eqn{x_i^{+}}) and negative
  (\eqn{x_i^{-}}) word is then weighted by the amplifiers
  (\eqn{x_i^{\uparrow}}) directly proceeding the positive
  or negative word.  Next, I consider amplification value,
  adding the assigned value \eqn{1/n-1} to increase the
  polarity relative to sentence length while ensuring that
  the polarity scores will remain between the values -1 and
  1.  This weighted value for each polarized word is then
  multiplied by -1 to the power of the number of negated
  (\eqn{x_i\neg}) words directly proceeding the positive or
  negative word. Last, these values are then summed and
  divided by the word count (\eqn{n}) yielding a polarity
  score (\eqn{\delta}) between -1 and 1.

  \deqn{\delta=\frac{\sum(x_i^{0},\quad x_i^{\uparrow} +
  x_i^{+}\cdot(-1)^{\sum(x_i\neg)},\quad x_i^{\uparrow} +
  x_i^{-}\cdot(-1)^{\sum(x_i\neg)})}{n}}

  Where:

  \deqn{x_i^{\uparrow}=\frac{1}{n-1}}
}
\note{
  The polarity score is dependent upon the polarity
  dictionary used. This function defaults to the word
  polarity word dictionary used by Hu, M., & Liu, B.
  (2004), however, this may not be appropriate for the
  context of children in a classroom.  The user may (is
  encouraged) to provide/augment the dictionary.  For
  instance the word "sick" in a high school setting may
  mean that something is good, whereas "sick" used by a
  typical adult indicates something is not right or
  negative connotation.

  Also note that \code{\link[qdap]{polarity}} assumes
  you've run \code{\link[qdap]{sentSplit}}.
}
\examples{
\dontrun{
(poldat <- with(DATA, polarity(state, person)))
with(DATA, polarity(state, list(sex, adult)))
names(poldat)
poldat$all
poldat$group
poldat2 <- with(mraja1spl, polarity(dialogue, list(sex, fam.aff, died)))
colsplit2df(poldat2$group)
plot(poldat)

(poldat2 <- with(rajSPLIT, polarity(dialogue, person)))
poldat2[["group"]][, "OL"] <- outlier.labeler(poldat2[["group"]][, "ave.polarity"])
poldat2[["all"]][, "OL"] <- outlier.labeler(poldat2[["all"]][, "polarity"])
head(poldat2[["group"]], 10)
truncdf(poldat2[["all"]], 20)
plot(poldat2)
plot(poldat2, nrow=4)
plot(poldat2, nrow=NULL)
}
}
\references{
  Hu, M., & Liu, B. (2004). Mining opinion features in
  customer reviews. National Conference on Artificial
  Intelligence.

  \url{http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
}
\seealso{
  \url{https://github.com/trestletech/Sermon-Sentiment-Analysis}
}
\keyword{polarity}
\keyword{sentiment,}

