\name{prop}
\alias{prop}
\title{Convert Raw Numeric Matrix or Data Frame to Proportions}
\usage{
  prop(mat, digits = 2, percent = FALSE, by.column = TRUE)
}
\arguments{
  \item{mat}{A numeric matrix or dataframe.}

  \item{digits}{Integer; number of decimal places to
  round.}

  \item{percent}{logical.  If TRUE output given as percent.
  If FALSE the output is proption.}

  \item{by.column}{logical.  If TRUE applies to the column.
  If FALSE applies by row.}
}
\value{
  Returns a matrix with proportionaly scaled values.
}
\description{
  Convert a raw matrix or dataframe to proprtions/percents.
  Divides each element of a column by the column sum.
}
\examples{
\dontrun{
y <- wfdf(DATA$state, DATA$person, stopwords = c("your", "yours"),
    margins = TRUE)
prop(wfm(wfdf = y), 4)       #as a proportion
prop(wfm(wfdf = y), 4, TRUE) #as a percentage
heatmap(prop(wfm(wfdf = y), 4))
wdstraj <- word_stats(rajSPLIT$dialogue, rajSPLIT$person)
prop(wdstraj$gts[, -1], 5)
}
}
\keyword{percent,}
\keyword{percentage}
\keyword{proportion,}

