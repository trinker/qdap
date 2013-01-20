\name{automated_readability_index}
\alias{automated_readability_index}
\alias{coleman_liau}
\alias{flesch_kincaid}
\alias{fry}
\alias{linsear_write}
\alias{SMOG}
\title{Readability Measures}
\usage{
  automated_readability_index(text.var,
    grouping.var = NULL, rm.incomplete = FALSE, ...)

  coleman_liau(text.var, grouping.var = NULL,
    rm.incomplete = FALSE, ...)

  SMOG(text.var, grouping.var = NULL, output = "valid",
    rm.incomplete = FALSE, ...)

  flesch_kincaid(text.var, grouping.var = NULL,
    rm.incomplete = FALSE, ...)

  fry(text.var, grouping.var = NULL, labels = "automatic",
    rm.incomplete = FALSE, ...)

  linsear_write(text.var, grouping.var = NULL,
    rm.incomplete = FALSE, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{rm.incomplete}{logical.  If TRUE removes incomplete
  sentences from the analysis.}

  \item{\ldots}{Other arguments passed to \code{endf}.}

  \item{output}{A character vector character string
  indicating output type. One of "valid" (default and
  congruent with McLaughlin's intent) or "all".}

  \item{labels}{A character vector character string
  indicating output type. One of \code{"automatic"}
  (default; adds labels automatically) or \code{"click"}
  (interactive).}
}
\value{
  Returns a dataframe with selected readability statistic
  by grouping variable(s).  The \code{frey} function
  returns a graphic representation of the readability.
}
\description{
  \code{automated_readability_index} - Apply Automated
  Readability Index to transcript(s) by zero or more
  grouping variable(s).

  \code{coleman_liau} - Apply Coleman Liau Index to
  transcript(s) by zero or more grouping variable(s).

  \code{SMOG} - Apply SMOG Readability to transcript(s) by
  zero or more grouping variable(s).

  \code{flesch_kincaid} - Flesch-Kincaid Readability to
  transcript(s) by zero or more grouping variable(s).

  \code{fry} - Apply Fry Readability to transcript(s) by
  zero or more grouping variable(s).

  \code{linsear_write} - Apply Linsear Write Readability to
  transcript(s) by zero or more grouping variable(s).
}
\note{
  Many of the indices (e.g. Automated Readability Index)
  are derived from word difficulty (letters per word) and
  sentence difficulty (words per sentence).  If you have
  not run the sentSplit function on your data the results
  may not be accurate.
}
\examples{
\dontrun{
with(rajSPLIT, automated_readability_index(dialogue, list(person, act)))
with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))

with(rajSPLIT, coleman_liau(dialogue, list(person, act)))
with(rajSPLIT, coleman_liau(dialogue, list(sex, fam.aff)))

with(rajSPLIT, SMOG(dialogue, list(person, act)))
with(rajSPLIT, SMOG(dialogue, list(sex, fam.aff)))

with(rajSPLIT, flesch_kincaid(dialogue, list(person, act)))
with(rajSPLIT, flesch_kincaid(dialogue, list(sex, fam.aff)))

(x <- with(rajSPLIT, fry(dialogue, list(sex, fam.aff))))
names(x)
with(rajSPLIT, fry(dialogue, list(sex, fam.aff), labels = "click"))

with(rajSPLIT, linsear_write(dialogue, list(person, act)))
with(rajSPLIT, linsear_write(dialogue, list(sex, fam.aff)))
}
}
\references{
  Coleman, M., & Liau, T. L. (1975). A computer readability
  formula designed for machine scoring. Journal of Applied
  Psychology, Vol. 60, pp. 283-284.

  Flesch R. (1948). A new readability yardstick. Journal of
  Applied Psychology. Vol. 32(3), pp. 221-233. doi:
  10.1037/h0057532.

  Gunning, T. G. (2003). Building Literacy in the Content
  Areas. Boston: Allyn & Bacon.

  McLaughlin, G. H. (1969). SMOG Grading: A New Readability
  Formula. Journal of Reading, Vol. 12(8), pp. 639-646.

  Senter, R. J., & Smith, E. A.. (1967) Automated
  readability index. Technical Report AMRLTR-66-220,
  University of Cincinnati, Cincinnati, Ohio.
}
\keyword{Automated}
\keyword{Coleman}
\keyword{Flesch-Kincaid,}
\keyword{Fry,}
\keyword{Index,}
\keyword{Liau,}
\keyword{Linsear}
\keyword{Readability}
\keyword{readability,}
\keyword{SMOG,}
\keyword{Write}

