\name{names2sex}
\alias{names2sex}
\title{Names to Gender Prediction}
\usage{
  names2sex(names.list, pred.sex = TRUE)
}
\arguments{
  \item{names.list}{Character vector containing first
  names.}

  \item{pred.sex}{logical.  If \code{TRUE} overlapping M/F
  names will be predicted based on highes cummulative
  frequency.}
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
names2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone))

names2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE)
}
}
\references{
  \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}

  \url{http://stackoverflow.com/a/818231/1000343}
}
\keyword{gender}
\keyword{name}

