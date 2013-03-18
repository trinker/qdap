\name{tot_plot}
\alias{tot_plot}
\title{Visualize Word Length by Turn of Talk}
\usage{
  tot_plot(dataframe, text.var, grouping.var = NULL,
    tot = TRUE, ylab = NULL, xlab = NULL, space = 0)
}
\arguments{
  \item{dataframe}{A dataframe that contains the text
  variable and optionally the grouping.var and tot
  variables.}

  \item{text.var}{The text variable (character string).}

  \item{grouping.var}{The grouping variables (character
  string).}

  \item{tot}{The turn of talk variable (character string).
  May be TRUE (assumes "tot" is the variable name), FALSE
  (use row numbers), or a character string of the turn of
  talk column.}

  \item{ylab}{Optional y label.}

  \item{xlab}{Optional x label.}

  \item{space}{The amount space between bars (ranging
  between 1 and 0).}
}
\value{
  Invisibly returns the ggplot2 object.
}
\description{
  Uses a bar graph to visualize patterns in sentence length
  and grouping variables by turn of talk.
}
\examples{
\dontrun{
dataframe <- sentSplit(DATA, "state")
tot_plot(dataframe, "state")
tot_plot(DATA, "state", tot=FALSE)
tot_plot(dataframe, "state", space=.03)
tot_plot(dataframe, "state", "sex")
tot_plot(mraja1, "dialogue", "fam.aff", tot=FALSE)
tot_plot(mraja1, "dialogue", c("sex", "fam.aff"), tot=FALSE)
}
}
\keyword{sentence,}
\keyword{split,}
\keyword{turn-of-talk}

