\name{pos}
\alias{pos}
\alias{pos.by}
\alias{pos.tags}
\title{Parts of Speech Tagging}
\usage{
  pos(text.var, parallel = FALSE, na.omit = FALSE,
    digits = 1, progress.bar = TRUE, percent = TRUE,
    zero.replace = 0, gc.rate = 10)

  pos.by(text.var, grouping.var = NULL, digits = 1,
    percent = TRUE, zero.replace = 0, ...)

  pos.tags(type = "pretty")
}
\arguments{
  \item{text.var}{The text variable}

  \item{parallel}{logical.  If TRUE attempts to run the
  function on multiple cores.  Note that this may not mean
  a speed boost if you have one core or if the data set is
  smaller as the cluster takes time to create.}

  \item{na.omit}{logical.  If TRUE missing values
  (]code{NA}) will be omitted.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}

  \item{progress.bar}{logical.  If TRUE attempts to provide
  a OS appropriate progress bar.  If parallel is TRUE this
  argument is ignored.  Note that setting this argument to
  TRUE may slow down the function.}

  \item{percent}{logical.  If TRUE output given as percent.
  If FALSE the output is proportion.}

  \item{zero.replace}{Value to replace 0 values with.}

  \item{gc.rate}{An integer value.  This is a necessary
  argument because of a problem with the garbage collection
  in the openNLP function that \code{pos} wraps.  Consider
  adjusting this argument upward if the error
  \code{java.lang.OutOfMemoryError} occurs.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{\ldots}{Other argument supplied to \code{pos}.}

  \item{type}{An optional character string giving the
  output of the pos tags. This must be one of the strings
  \code{"pretty"} (a left justified version of the output
  optimized for viewing but not good for export),
  \code{"matrix"} (a matrix version of the output),
  \code{"dataframe"}\\ \code{"df"} (a dataframe version of
  the output), \code{"all"} (a list of all three of the
  previous output types).}
}
\value{
  pos returns a list of 4: \item{text}{The original text}
  \item{POStagged}{The original words replaced with parts
  of speech in context.} \item{POSprop}{Dataframe of the
  proportion of parts of speech by row.}
  \item{POSfreq}{Dataframe of the frequency of parts of
  speech by row.} \item{POSrnp}{Dataframe of the frequency
  and proportions of parts of speech by row.}
  \item{percent}{The value of percent used for plotting
  purposes.} \item{zero.replace}{The value of zero.replace
  used for plotting purposes.}

  pos.by returns a list of 6: \item{text}{The original
  text} \item{POStagged}{The original words replaced with
  parts of speech in context.} \item{POSprop}{Dataframe of
  the proportion of parts of speech by row.}
  \item{POSfreq}{Dataframe of the frequency of parts of
  speech by row.} \item{POSrnp}{Dataframe of the frequency
  and proportions of parts of speech by row.}
  \item{pos.by.prop}{Dataframe of the proportion of parts
  of speech by grouping variable.}
  \item{pos.by.freq}{Dataframe of the frequency of parts of
  speech by grouping variable.} \item{pos.by.rnp}{Dataframe
  of the frequency and proportions of parts of speech by
  grouping variable.} \item{percent}{The value of percent
  used for plotting purposes.} \item{zero.replace}{The
  value of zero.replace used for plotting purposes.}
}
\description{
  \code{pos} - Apply part of speech tagger to
  transcript(s).

  \code{pos.by} - Apply part of speech tagger to
  transcript(s) by zero or more grouping variable(s).

  \code{pos.tags} - Useful for interpreting the parts of
  speech tags created by pos and pos.by.
}
\examples{
\dontrun{
posdat <- pos(DATA$state)
str(posdat)
names(posdat)
posdat$text       #original text
posdat$POStagged  #words replaced with parts of speech
posdat$POSprop    #proportion of parts of speech by row
posdat$POSfreq    #frequency of parts of speech by row

pos(DATA$state, parallel = TRUE) # not always useful

#use pos.tags to interpret part of speech tags used by pos & pos.by
pos.tags()
pos.tags("matrix")
pos.tags("dataframe")
pos.tags("df")
pos.tags("all")

posbydat <- with(DATA, pos.by(state, sex))
names(posbydat)
posbydat
posbydat$pos.by.prop
(POSby <- with(DATA, pos.by(state, list(adult, sex))))
plot(POSby, values = TRUE, digits = 2)
#or more quickly - reuse the output from before
with(DATA, pos.by(posbydat, list(adult, sex)))
}
}
\references{
  \href{openNLP}{http:/opennlp.apache.org}
}
\seealso{
  \code{\link[openNLP]{tagPOS}}
}
\keyword{parts-of-speech}

