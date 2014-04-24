\name{sentiment_frame}
\alias{sentiment_frame}
\title{Power Score (Sentiment Analysis)}
\usage{
sentiment_frame(positives, negatives, pos.weights = 1, neg.weights = -1,
  envir = TRUE)
}
\arguments{
  \item{positives}{A character vector of positive words.}

  \item{negatives}{A character vector of negative words.}

  \item{pos.weights}{A vector of weights to weight each
  positive word by. Length must be equal to length of
  \code{postives} or length 1 (if 1 weight will be
  recycled).}

  \item{neg.weights}{A vector of weights to weight each
  negative word by. Length must be equal to length of
  \code{negatives} or length 1 (if 1 weight will be
  recycled).}

  \item{envir}{logical.  If \code{TRUE} a lookup table (a
  dataframe within an environment) is produced rather than
  a data.frame.}
}
\description{
\code{sentiment_frame} - Generate a sentiment lookup
environment or data.frame for use with the \code{xxx.frame}
argument of various sentiment functions function.
}

