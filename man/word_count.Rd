\name{word_count}
\alias{char_table}
\alias{character_count}
\alias{character_table}
\alias{wc}
\alias{word_count}
\title{Word Counts}
\usage{
  word_count(text.var, byrow = TRUE, missing = NA,
    digit.remove = TRUE, names = FALSE)

  wc(text.var, byrow = TRUE, missing = NA,
    digit.remove = TRUE, names = FALSE)

  character_count(text.var, byrow = TRUE, missing = NA,
    apostrophe.remove = TRUE, digit.remove = TRUE,
    count.space = FALSE)

  character_table(text.var, grouping.var, percent = TRUE,
    prop.by.row = TRUE, zero.replace = 0, digits = 2, ...)

  char_table(text.var, grouping.var, percent = TRUE,
    prop.by.row = TRUE, zero.replace = 0, digits = 2, ...)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{byrow}{logical.  If \code{TRUE} counts by row, if
  \code{FALSE} counts all words.}

  \item{missing}{Value to insert for missing values (empty
  cells).}

  \item{digit.remove}{logical.  If \code{TRUE} removes
  digits before counting words.}

  \item{names}{logical.  If \code{TRUE} the sentences are
  given as the names of the counts.}

  \item{apostrophe.remove}{logical.  If \code{TRUE}
  apostrophes will be counted in the character count.}

  \item{count.space}{logical.  If \code{TRUE} spaces are
  counted as characters.}

  \item{prop.by.row}{logical.  If \code{TRUE} applies
  proportional to the row. If \code{FALSE} applies by
  column.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{prop}}.}

  \item{percent}{logical.  If \code{TRUE} output given as
  percent.  If \code{FALSE} the output is proportion.}

  \item{zero.replace}{Value to replace 0 values with.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}
}
\value{
  \code{word_count} - returns a word count by row or total.

  \code{character_count} - returns a character count by row
  or total.

  \code{character_table} - returns a list: dataframe of
  character counts by grouping variable.
  \item{raw}{Dataframe of the frequency of characters by
  grouping variable.} \item{prop}{Dataframe of the
  proportion of characters by grouping variable.}
  \item{rnp}{Dataframe of the frequency and proportions of
  characters by grouping variable.} \item{percent}{The
  value of percent used for plotting purposes.}
  \item{zero.replace}{The value of zero.replace used for
  plotting purposes.}
}
\description{
  \code{word_count} - Transcript apply word counts.

  \code{character_count} - Transcript apply character
  counts.

  \code{character_table} - Computes a table of character
  counts by grouping . variable(s).
}
\note{
  wc is a convenient short hand for word_count.
}
\examples{
\dontrun{
# WORD COUNT
word_count(DATA$state)
wc(DATA$state)
word_count(DATA$state, names = TRUE)
word_count(DATA$state, byrow=FALSE, names = TRUE)
sum(word_count(DATA$state))

# CHARACTER COUNTS
character_count(DATA$state)
character_count(DATA$state, byrow=FALSE)
sum(character_count(DATA$state))

# CHARACTER TABLE
x <- character_table(DATA$state, DATA$person)
plot(x)
plot(x, label = TRUE)
plot(x, label = TRUE, text.color = "red")
plot(x, label = TRUE, lab.digits = 1, zero.replace = "PP7")
x$raw[, 1:20]
x$prop[, 1:8]
x$rnp[, 1:8]

## char_table(DATA$state, DATA$person)
## char_table(DATA$state, DATA$person, percent = TRUE)
## character_table(DATA$state, list(DATA$sex, DATA$adult))

library(ggplot2);library(reshape2)
dat <- character_table(DATA$state, list(DATA$sex, DATA$adult))
dat2 <- colsplit2df(melt(dat$raw), keep.orig = TRUE)
head(dat2, 15)
dat3 <- dat2[rep(seq_len(dim(dat2)[1]), dat2[, 5]), -5]


ggplot(data = dat2, aes(y = variable, x = value, colour=sex)) +
    facet_grid(adult~.) +
    geom_line(size=1, aes(group =variable), colour = "black") +
    geom_point()

ggplot(data = dat3, aes(x = variable, fill = variable)) +
    geom_bar() +
    facet_grid(sex ~ adult, margins = TRUE) +
    theme(legend.position="none")
}
}
\seealso{
  \code{\link[qdap]{syllable_count}}

  \code{\link[qdap]{prop}}
}
\keyword{character-count}
\keyword{word-count,}
