\name{cm_df.temp}
\alias{cm_df.temp}
\title{Break Transcript Dialogue into Blank Code Matrix}
\usage{
  cm_df.temp(dataframe, text.var, codes = NULL, csv = TRUE,
    file.name = NULL, transpose = FALSE, strip = FALSE,
    ...)
}
\arguments{
  \item{dataframe}{A dataframe containing a text variable.}

  \item{text.var}{The name of the text variable.}

  \item{codes}{Optional list of codes.}

  \item{csv}{logical.  If TRUE creates a csv in the working
  directory.}

  \item{file.name}{The name of the csv file.  If NULL
  defaults to the dataframe name.}

  \item{transpose}{logical.  If TRUE transposes the
  dataframe so that the text is across the top.}

  \item{strip}{logical.  If TRUE all punctuation is
  removed.}

  \item{\ldots}{Other arguments passed to strip.}
}
\value{
  Generates a dataframe, and optional csv file, of
  individual words while maintaining demographic
  information.  If a vector of codes is provided the
  outcome is a matrix of words used by codes filled with
  zeros.  This dataframe is useful for dummy coded (1-yes
  code exists; 2-no it does not) representation of data and
  can be used for visualizations and statistical analysis.
}
\description{
  Breaks transcript dialogue into words while retaining the
  demographic factors associate with each word.  The codes
  argument provides a matrix of zeros that can serve as a
  dummy coded matrix of codes per word.
}
\examples{
\dontrun{
codes <- qcv(dc, sf, wes, pol, rejk, lk, azx, mmm)
out1 <- cm_df.temp(DATA, "state", codes)
head(out1, 15)
out2 <- cm_df.temp(DATA, "state", codes, transpose = TRUE)
out2[, 1:10]
out3 <- cm_df.temp(raj.act.1, "dialogue", codes)
head(out3, 15)
out4 <- cm_df.temp(raj.act.1, "dialogue", codes, transpose = TRUE)
out4 [, 1:8]
}
}
\references{
  Miles, M. B. & Huberman, A. M. (1994). An expanded
  sourcebook: Qualitative data analysis. 2nd ed. Thousand
  Oaks, CA: SAGE Publications.
}
\seealso{
  \code{\link{cm_range2long}},
  \code{\link{cm_df.transcript}}, \code{\link{cm_df.fill}}
}
\keyword{coding}

