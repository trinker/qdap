\name{syllable.sum}
\alias{combo_syllable.sum}
\alias{polysyllable.sum}
\alias{syllable.count}
\alias{syllable.sum}
\title{Syllabication}
\usage{
  syllable.sum(text.var, parallel = FALSE)

  syllable.count(text, remove.bracketed = TRUE,
    algorithm.report = FALSE)

  polysyllable.sum(text.var, parallel = FALSE)

  combo_syllable.sum(text.var, parallel = FALSE)
}
\arguments{
  \item{text.var}{The text variable}

  \item{parallel}{logical.  If \code{TRUE} attempts to run
  the function on multiple cores.  Note that this may not
  mean a speed boost if you have one core or if the data
  set is smaller as the cluster takes time to create.}

  \item{text}{A single character vector of text.}

  \item{remove.bracketed}{logical.  If \code{TRUE} brackets
  are removed from the analysis.}

  \item{algorithm.report}{logical.  If \code{TRUE}
  generates a report of words not found in the dictionary
  (i.e., syllables were calculated with an algorithm).}
}
\value{
  \code{syllable.sum} - returns a vector of syllable counts
  per row.

  \code{syllable.count} - returns a dataframe of syllable
  counts and algorithm/dictionary uses and, optionally, a
  report of words not found in the dictionary.

  \code{polysyllable.sum} - returns a vector of
  polysyllable counts per row.

  \code{combo_syllable.sum} - returns a dataframe of
  syllable and polysyllable counts per row.
}
\description{
  \code{syllable.sum} - Count the number of syllables per
  row of text.

  \code{syllable.count} - Count the number of syllables in
  a single text string.

  \code{polysyllable.sum} - Count the number of
  polysyllables per row of text.

  \code{combo_syllable.sum} - Count the number of both
  syllables and polysyllables per row of text.
}
\details{
  The worker function of all the syllable functions is
  \code{\link[qdap]{syllable.count}}, though it is not
  intended for direct use on a transcript.  This function
  relies on a combined dictionary lookup (based on the
  Nettalk Corpus (Sejnowski & Rosenberg, 1987)) and backup
  algorithm method.
}
\examples{
\dontrun{
syllable.count("Robots like Dason lie.")
syllable.count("Robots like Dason lie.", algorithm.report = TRUE)
syllable.sum(DATA$state)
polysyllable.sum(DATA$state)
combo_syllable.sum(DATA$state)
}
}
\references{
  Sejnowski, T.J., and Rosenberg, C.R. (1987). "Parallel
  networks that learn to pronounce English text" in Complex
  Systems, 1, 145-168.
}
\keyword{polysyllable}
\keyword{syllabication,}
\keyword{syllable,}

