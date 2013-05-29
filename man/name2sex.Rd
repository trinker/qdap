\name{name2sex}
\alias{name2sex}
\title{Names to Gender Prediction}
\usage{
  name2sex(names.list, pred.sex = TRUE,
    fuzzy.match = pred.sex, USE.NAMES = FALSE)
}
\arguments{
  \item{names.list}{Character vector containing first
  names.}

  \item{pred.sex}{logical.  If \code{TRUE} overlapping M/F
  names will be predicted based on highest cummulative
  frequency.  If \code{FALSE} the overlapping names will be
  denoted with a \code{"B"}.}

  \item{fuzzy.match}{ligical.  If \code{TRUE} uses
  Levenshtein edit distance from \code{\link[base]{agrep}}
  to predict gender from the closest name match starting
  with the same letter.  This is computationally intensive
  and should not be used on larger vectors.  Defaults to
  \code{pred.sex}.}

  \item{USE.NAMES}{logical.  If \code{TRUE} names.list is
  used to name the gender vector.}
}
\value{
  Returns a vector of predicted gender (M/F) based on first
  name.
}
\description{
  Predict gender from U.S. names (based on 1990 U.S. census
  data).
}
\examples{
\dontrun{
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew))

name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE)

name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE, TRUE)

name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew), TRUE, FALSE)
}
}
\author{
  Dason Kurkiewicz and Tyler Rinker
  <tyler.rinker@gmail.com>.
}
\references{
  \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}

  \url{http://stackoverflow.com/a/818231/1000343}

  \url{http://www.talkstats.com/showthread.php/31660}
}
\seealso{
  \code{\link[base]{agrep}}
}
\keyword{gender}
\keyword{name}

