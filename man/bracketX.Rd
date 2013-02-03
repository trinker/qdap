\name{bracketX}
\alias{bracketX}
\alias{bracketXtract}
\alias{genX}
\alias{genXtract}
\title{Bracket Parsing}
\usage{
  bracketX(text.var, bracket = "all", missing = NULL,
    names = FALSE)

  bracketXtract(text.var, bracket = "all", with = FALSE,
    merge = TRUE)

  genX(text.var, left, right, missing = NULL,
    names = FALSE)

  genXtract(text.var, left, right, with = FALSE,
    merge = TRUE)
}
\arguments{
  \item{text.var}{The text variable}

  \item{bracket}{The type of bracket (and encased text) to
  remove.  This is one or more of the strings
  \code{"curly"}, \code{"square"}, \code{"round"},
  \code{"angle"} and \code{"all"}.  These strings
  correspond to: \{, [, (, < or all four types.}

  \item{missing}{Value to assign to empty cells.}

  \item{names}{logical.  If TRUE the sentences are given as
  the names of the counts.}

  \item{with}{logical.  If TRUE returns the brackets and
  the bracketed text.}

  \item{merge}{logical.  If TRUE the results of each
  bracket type will be merged by sentence.  FALSE returns a
  named list of lists of vectors of bracketed text per
  bracket type.}

  \item{left}{A vector of character or numeric symbols as
  the left edge to extract.}

  \item{right}{A vector of character or numeric symbols as
  the right edge to extract.}
}
\value{
  \code{bracketX} - returns a vector of text with brackets
  removed.

  \code{bracketXtract} - returns a list of vectors of
  bracketed text.

  \code{genXtract} - returns a vector of text with checks
  removed.

  \code{genX} - returns a list of vectors of removed text.
}
\description{
  \code{bracketX} - Apply bracket removal to character
  vectors.

  \code{bracketXtract} - Apply bracket extraction to
  character vectors.

  \code{genX} - Apply general chunk removal to character
  vectors.

  \code{genXtract} - Apply general chunk extraction to
  character vectors.
}
\section{Warning}{
  the \code{gen} functions are more flexible than the
  \code{bracket} functions but are not compatible with
  special \code{\link[base]{regex}} characters.  It is
  recommended that the researcher use the \code{bracket}
  functions.
}
\examples{
\dontrun{
examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
    .Label = c("bob", "greg", "sue"), class = "factor"), text =
    c("I love chicken [unintelligible]!",
    "Me too! (laughter) It's so good.[interrupting]",
    "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
    c("person", "text"), row.names = c(NA, -4L), class = "data.frame")

examp
bracketX(examp$text, "square")
bracketX(examp$text, "curly")
bracketX(examp$text, c("square", "round"))
bracketX(examp$text)


bracketXtract(examp$text, "square")
bracketXtract(examp$text, "curly")
bracketXtract(examp$text, c("square", "round"))
bracketXtract(examp$text, c("square", "round"), merge = FALSE)
bracketXtract(examp$text)
bracketXtract(examp$text, with = TRUE)

paste2(bracketXtract(examp$text, "curly"), " ")

x <- c("Where is the /big dog#?",
    "I think he's @arunning@b with /little cat#.")
genXtract(x, c("/", "@a"), c("#", "@b"))

x <- c("Where is the L1big dogL2?",
    "I think he's 98running99 with L1little catL2.")
genXtract(x, c("L1", 98), c("L2", 99))

DATA$state  #notice number 1 and 10
genX(DATA$state, c("is", "we"), c("too", "on"))
}
}
\author{
  Martin Morgan and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
  \url{http://stackoverflow.com/questions/8621066/remove-text-inside-brackets-parens-and-or-braces}
}
\seealso{
  \code{\link[base]{regex}}
}
\keyword{bracket-remove,}
\keyword{bracket,}
\keyword{curly-braces}
\keyword{parenthesis,}

