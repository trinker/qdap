\name{bracketX}
\alias{bracketX}
\alias{bracketXtract}
\title{Bracket Parsing}
\usage{
  bracketX(text.var, bracket = "all", missing = NULL,
    names = FALSE)

  bracketXtract(text.var, bracket = "all", with = FALSE)
}
\arguments{
  \item{text.var}{The text variable}

  \item{bracket}{The type of bracket (and encased text) to
  remove.  This is one of the strings \code{"curly"},
  \code{"square"}, \code{"round"}, \code{"angle"} and
  \code{"all"}.  These strings correspond to: \{, [, (, <
  or all four types.}

  \item{missing}{Value to assign to empty cells.}

  \item{names}{logical.  If TRUE the sentences are given as
  the names of the counts.}

  \item{with}{logical.  If TRUE returns the brackets and
  the bracketted text.}
}
\value{
  \code{bracketX} - returns a vector of text with brackets
  removed.

  \code{bracketXtract} - returns a list of vectors of
  bracketed text.
}
\description{
  \code{bracketX} - Apply bracket removal to character
  vectors.

  \code{bracketXtract} - Apply bracket extraction to
  character vectors.
}
\examples{
\dontrun{
examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
    .Label = c("bob", "greg", "sue"), class = "factor"), text =
    c("I love chicken [unintelligible]!",
    "Me too! (laughter) It's so good.[interupting]",
    "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
    c("person", "text"), row.names = c(NA, -4L), class = "data.frame")

examp
bracketX(examp$text, "square")
bracketX(examp$text, "curly")
bracketX(examp$text)

examp
bracketXtract(examp$text, "square")
bracketXtract(examp$text, "curly")
bracketXtract(examp$text)
bracketXtract(examp$text, with = TRUE)

paste2(bracketXtract(examp$text, "curly"), " ")
}
}
\author{
  Martin Morgan and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
  \url{http://stackoverflow.com/questions/8621066/remove-text-inside-brackets-parens-and-or-braces}
}
\keyword{bracket-remove,}
\keyword{bracket,}
\keyword{curly-braces}
\keyword{parenthesis,}

