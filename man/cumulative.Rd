% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumulative.R, R/end_mark.R, R/formality.R,
%   R/lexical_classification.R, R/polarity.R, R/syllable_sum.R
\name{cumulative}
\alias{cumulative}
\alias{cumulative.end_mark}
\alias{cumulative.formality}
\alias{cumulative.pos}
\alias{cumulative.pos_by}
\alias{cumulative.animated_formality}
\alias{cumulative.lexical_classification}
\alias{cumulative.animated_lexical_classification}
\alias{cumulative.polarity}
\alias{cumulative.animated_polarity}
\alias{cumulative.syllable_freq}
\alias{cumulative.combo_syllable_sum}
\title{Cumulative Scores}
\usage{
cumulative(x, ...)

\method{cumulative}{end_mark}(x, ...)

\method{cumulative}{formality}(x, ...)

\method{cumulative}{pos}(x, ...)

\method{cumulative}{pos_by}(x, ...)

\method{cumulative}{animated_formality}(x, ...)

\method{cumulative}{lexical_classification}(x, ...)

\method{cumulative}{animated_lexical_classification}(x, ...)

\method{cumulative}{polarity}(x, ...)

\method{cumulative}{animated_polarity}(x, ...)

\method{cumulative}{syllable_freq}(x, ...)

\method{cumulative}{combo_syllable_sum}(x, ...)
}
\arguments{
\item{x}{A qdap object with an accompanying \code{cumulative} method.}

\item{\ldots}{ignored}
}
\description{
\code{cumulative} - Generate rolling/cumulative scores for select \pkg{qdap} 
objects.
}
