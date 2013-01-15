\name{imperative}
\alias{imperative}
\title{Intuitively Remark Sentences as Imperative}
\usage{
  imperative(dataframe, person.var, text.var,
    lock.incomplete = FALSE, additional.names = NULL,
    warning = FALSE)
}
\arguments{
  \item{dataframe}{A data.frame object.}

  \item{person.var}{The person variable.}

  \item{text.var}{The text variable.}

  \item{lock.incomplete}{logical.  If TRUE locks incomplete
  sentences (sentences ending with "|") from being marked
  as imperative.}

  \item{additional.names}{Additional names that may be used
  in a command (people in the context that do not speak).}

  \item{warning}{logical.  If TRUE provides comma warnings
  (sentences that contain numerous commas that may be
  handled incorrectly by the algorithm).}
}
\value{
  Returns a dataframe with a text variable indicating
  imperative senteces.  Imperative sentences are marked
  with * followed by the original end mark.
}
\description{
  Automatic imperative remarking.
}
\note{
  The algorithm used by \code{imperative} is sentive to
  English language dialects and types.  Commas can indicate
  a choppy sentence and may indicate a false postive.
}
\examples{
\dontrun{
DATA3 <- data.frame(name=c('sue', rep(c('greg', 'tyler', 'phil', 'sue'), 2)),
    statement=c('go get it|', 'I hate to read.', 'Stop running!', 'I like it!',
    'You are terrible!', "Don't!", 'Greg, go to the red, brick office.',
    'Tyler go to the gym.', "Alex don't run."), stringsAsFactors = FALSE)
imperative(DATA3, 'name', 'statement', , c('Alex'))
imperative(DATA3, 'name', 'statement', lock.incomplete = TRUE, c('Alex'))
imperative(DATA3, 'name', 'statement', , c('Alex'), warning=TRUE)
X <- imperative(mraja1spl, 'person', 'dialogue', warning=FALSE)
truncdf(X[, -7], 60)
strwrap(X$dialogue)
}
}

