% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude.R
\name{exclude}
\alias{exclude}
\alias{exclude.TermDocumentMatrix}
\alias{exclude.DocumentTermMatrix}
\alias{exclude.wfm}
\alias{exclude.list}
\alias{exclude.default}
\alias{\%ex\%}
\title{Exclude Elements From a Vector}
\usage{
exclude(word.list, ...)

\method{exclude}{TermDocumentMatrix}(word.list, ...)

\method{exclude}{DocumentTermMatrix}(word.list, ...)

\method{exclude}{wfm}(word.list, ...)

\method{exclude}{list}(word.list, ...)

\method{exclude}{default}(word.list, ...)

word.list \%ex\% ...
}
\arguments{
\item{word.list}{A list/vector of words/terms, a \code{\link[qdap]{wfm}}, 
\code{\link[tm]{DocumentTermMatrix}}, or \code{\link[tm]{TermDocumentMatrix}}
to exclude from.}

\item{\dots}{A vector (character/numeric) if element(s) to be excluded from 
the \code{word.list}.}
}
\value{
Returns a vector with the excluded terms removed.
}
\description{
\code{exclude} - Quickly exclude words from a word list

\code{\%ex\%} - Binary operator version of \code{\link[qdap]{exclude}} .
}
\examples{
\dontrun{
exclude(1:10, 3, 4)
exclude(1:10, 3:4)
Top25Words
exclude(Top25Words, qcv(the, of, and))
exclude(Top25Words, "the", "of", "an")

#Using with term_match and termco 
terms <- term_match(DATA$state, qcv(th), FALSE) 
exclude(terms, "truth")  
#all together
termco(DATA$state, DATA$person, exclude(term_match(DATA$state, qcv(th), 
    FALSE), "truth"))

MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)

## Works with wfm
dat <- wfm(DATA$state, DATA$person)
the.no <- term_match(DATA$state, c("the", "no"))
exclude(dat, unlist(the.no))

## Works with tm's TermDocumentMatrix/DocumentTermMatrix
dat2 <- as.dtm(DATA$state, DATA$person)
out.dtm <- exclude(dat2, unlist(the.no))
tm::inspect(out.dtm)

dat3 <- as.tdm(DATA$state, DATA$person)
out.tdm <- exclude(dat3, unlist(the.no))
tm::inspect(out.tdm)
}
}
