\name{tdm}
\alias{apply_as_tm}
\alias{df2tm_corpus}
\alias{dtm}
\alias{tdm}
\alias{tm2qdap}
\alias{tm_corpus2df}
\title{tm Package Compatability Tools: Apply to or Convert to/from Term Document
Matrix or Document Term Matrix}
\usage{
tdm(text.var, grouping.var = NULL, vowel.check = TRUE, ...)

dtm(text.var, grouping.var = NULL, vowel.check = TRUE, ...)

tm2qdap(x)

apply_as_tm(wfm.obj, tmfun, ..., to.qdap = TRUE)

tm_corpus2df(tm.corpus, col1 = "docs", col2 = "text")

df2tm_corpus(text.var, grouping.var = NULL, ...)
}
\arguments{
  \item{text.var}{The text variable or a
  \code{\link[qdap]{wfm}} object.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{\ldots}{If \code{tdm} or \code{dtm} - Other
  arguments passed to \code{wfm}.  If \code{apply_as_tm} -
  Other arguments passed to functions used on the tm
  package's \code{"TermDocumentMatrix"}.  If
  \code{df2tm_corpus} - Other arguments passed to the tm
  package's \code{\link[tm]{Corpus}}.}

  \item{vowel.check}{logical.  Should terms without vowels
  be remove?}

  \item{x}{A
  \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.}

  \item{wfm.obj}{A \code{\link[qdap]{wfm}} object.}

  \item{tmfun}{A function applied to a
  \code{\link[tm]{TermDocumentMatrix}} object.}

  \item{to.qdap}{logical.  If \code{TRUE} should
  \code{\link[qdap]{wfm}} try to coerce the output back to
  a qdap object.}

  \item{tm.corpus}{A \code{\link[tm]{Corpus}} object.}

  \item{col1}{Name for column 1 (the vector elements).}

  \item{col2}{Name for column 2 (the names of the
  vectors).}
}
\value{
\code{tdm} - Returns a
\code{\link[tm]{TermDocumentMatrix}}.

\code{dtm} - Returns a
\code{\link[tm]{DocumentTermMatrix}}.

\code{tm2qdap} - Returns a \code{\link[qdap]{wfm}} object
or \code{\link[qdap]{wfm_weight}} object.

\code{apply_as_tm} - Applies a tm oriented function to a
\code{\link[qdap]{wfm}} and attempts to simplify back to a
\code{\link[qdap]{wfm}} or \code{\link[qdap]{wfm_weight}}
format.

\code{tm_corpus2df} - Converts a \code{\link[tm]{Corpus}}
and returns a qdap oriented dataframe.

\code{df2tm_corpus} - Converts a qdap oriented dataframe
and returns a \code{\link[tm]{Corpus}}.
}
\description{
\code{tdm} - Create term document matrices from raw text or
\code{\link[qdap]{wfm}} for use with other text analysis
packages.

\code{dtm} - Create document term matrices from raw text or
\code{\link[qdap]{wfm}} for use with other text analysis
packages.

\code{tm2qdap} - Convert the \code{tm} package's
\code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}
to \code{\link[qdap]{wfm}}.

\code{apply_as_tm} - Apply functions intended to be used on
the \code{tm} package's
\code{\link[tm]{TermDocumentMatrix}} to a
\code{\link[qdap]{wfm}} object.

\code{tm_corpus2df} - Convert a tm package corpus to a
dataframe.

\code{df2tm_corpus} - Convert a qdap dataframe to a tm
package \code{\link[tm]{Corpus}}.
}
\details{
Produces output that is identical to the \code{tm}
package's \code{\link[tm]{TermDocumentMatrix}},
\code{\link[tm]{DocumentTermMatrix}},
\code{\link[tm]{Corpus}} or allows convenient inteface
between the qdap and tm packages.
}
\examples{
\dontrun{
dtm(DATA$state, DATA$person)
tdm(DATA$state, DATA$person)

x <- wfm(DATA$state, DATA$person)
tdm(x)
dtm(x)
library(tm)
plot(tdm(x))

pres <- tdm(pres_debates2012$dialogue, pres_debates2012$person)
plot(pres, corThreshold = 0.8)
pres
(pres2 <- removeSparseTerms(pres, .3))
plot(pres2, corThreshold = 0.95)

## Latent Semantic Analysis
library(lsa)
lsa(tdm(x), dims=dimcalc_share())
lsa(tdm(DATA$state, DATA$person), dims=dimcalc_share())

## Correspondence Analysis
library(ca)

dat <- pres_debates2012
dat <- dat[dat$person \%in\% qcv(ROMNEY, OBAMA), ]

speech <- stemmer(dat$dialogue)
mytable1 <- with(dat, tdm(speech, list(person, time), stopwords = Top25Words))

fit <- ca(mytable1)
summary(fit)
plot(fit)
plot3d.ca(fit, labels=1)


mytable2 <- with(dat, tdm(speech, list(person, time), stopwords = Top200Words))

fit2 <- ca(mytable2)
summary(fit2)
plot(fit2)
plot3d.ca(fit2, labels=1)

## Topic Models
# Example 1
library(topicmodels); library(tm)

# Generate stop words based on short words, frequent words and contractions
shorts <- all_words(pres_debates2012)[,1][nchar(all_words(
    pres_debates2012)[,1]) < 4]

SW <- c(shorts, qdapDictionaries::contractions[, 1],
    qdapDictionaries::Top200Words,
    "governor", "president", "mister", "obama","romney")

DocTermMat <- with(pres_debates2012, dtm(dialogue, person, stopwords = SW))
DocTermMat <- removeSparseTerms(DocTermMat,0.999)
DocTermMat <- DocTermMat[rowSums(as.matrix(DocTermMat))> 0,]

lda.model <- LDA(DocTermMat, 5)

(topics <- posterior(lda.model, DocTermMat)$topics)
terms(lda.model,20)

# Example 2
DocTermMat2 <- with(pres_debates2012, dtm(dialogue, list(person, time), stopwords = SW))
DocTermMat2 <- removeSparseTerms(DocTermMat2,0.95)
DocTermMat2 <- DocTermMat2[rowSums(as.matrix(DocTermMat2))> 0,]

lda.model2 <- LDA(DocTermMat2, 6)

(topics2 <- posterior(lda.model2, DocTermMat2)$topics)
terms(lda.model2,20)
qheat(topics2, high="blue", low="yellow", by.col=F)

## tm Matrices to wfm
library(tm)
data(crude)

## A Term Document Matrix Conversion
(tm_in <- TermDocumentMatrix(crude, control = list(stopwords = TRUE)))
converted <- tm2qdap(tm_in)
head(converted)
summary(converted)

## A Document Term Matrix Conversion
(dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
summary(tm2qdap(dtm_in))

## `apply_as_tm` Examples
## Create a wfm
a <- with(DATA, wfm(state, list(sex, adult)))
summary(a)

## Apply functions meant for a tm TermDocumentMatrix
out <- apply_as_tm(a, tm:::removeSparseTerms, sparse=0.6)
summary(out)

apply_as_tm(a, tm:::Dictionary)
apply_as_tm(a, tm:::dissimilarity, method = "cosine")
apply_as_tm(a, tm:::findAssocs, "computer", .8)
apply_as_tm(a, tm:::findFreqTerms, 2, 3)
apply_as_tm(a, tm:::Zipf_plot)
apply_as_tm(a, tm:::Heaps_plot)
apply_as_tm(a, tm:::plot.TermDocumentMatrix, corThreshold = 0.4)

library(proxy)
apply_as_tm(a, tm:::weightBin)
apply_as_tm(a, tm:::weightBin, to.qdap = FALSE)
apply_as_tm(a, tm:::weightSMART)
apply_as_tm(a, tm:::weightTfIdf)

## Convert tm Corpus to Dataframe
## A tm Corpus
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),
    readerControl = list(reader = readReut21578XML))

## Convert to dataframe
corp_df <- tm_corpus2df(reuters)
htruncdf(corp_df)

## Apply a qdap function
out <- formality(corp_df$text, corp_df$docs)
plot(out)

## Convert a qdap dataframe to tm package Corpus
(x <- with(DATA2, df2tm_corpus(state, list(person, class, day))))
library(tm)
inspect(x)
class(x)

(y <- with(pres_debates2012, df2tm_corpus(dialogue, list(person, time))))
}
}
\seealso{
\code{\link[tm]{DocumentTermMatrix}},
\code{\link[tm]{Corpus}},
\code{\link[tm]{TermDocumentMatrix}}
}

