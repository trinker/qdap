\name{capitalizer}
\alias{capitalizer}
\title{Capitalize Select Words}
\usage{
  capitalizer(text, caps.list = NULL, I.list = TRUE,
    apostrophe.remove = FALSE)
}
\arguments{
  \item{text}{A vector of words (generally from bag.o.words
  or breaker).}

  \item{caps.list}{A list of words to capitalize.}

  \item{I.list}{logical.  If TRUE capitalizes I words and
  contractions.}

  \item{apostrophe.remove}{logical, asking if apostrophes
  have been removed.  If TRUE will try to insert
  apostrophe's back into words appropriately.}
}
\value{
  Returns a vector of capitalized words based on supplied
  capitalization arguments.
}
\description{
  A helper function for word_list that allows the user to
  supply vectors of words to be capitalized.
}
\note{
  Not intended for general use.  Acts as a helper function
  to several qdap functions.
}
\examples{
\dontrun{
capitalizer(bag.o.words("i like it but i'm not certain"), "like")
capitalizer(bag.o.words("i like it but i'm not certain"), "like", FALSE)
}
}

