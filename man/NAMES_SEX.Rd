\docType{data}
\name{NAMES_SEX}
\alias{NAMES_SEX}
\title{First Names and Predictive Gender (U.S.)}
\format{A data frame with 5163 rows and 3 variables}
\description{
  A truncated version of the \code{link[qdap]{NAMES}})
  dataset used for predicting.
}
\details{
  \itemize{ \item name. A first name.  \item gender2.
  Gender of the combined male/female list with "B" in place
  of overlapping (M/F) names.  \item pred.sex. Predicted
  gender of the names with B's in \code{gender2} repalced
  with the gender that had a higher \code{per.freq}. }
}
\references{
  \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
}
\keyword{datasets}

