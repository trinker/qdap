\name{termco.a}
\alias{term.match}
\alias{termco.a}
\alias{termco.d}
\alias{termco2mat}
\title{Search For and Count Terms}
\usage{
  termco.a(text.var, grouping.var = NULL, match.list,
    short.term = TRUE, ignore.case = TRUE, elim.old = TRUE,
    output = "percent", digits = 2,
    apostrophe.remove = FALSE, char.keep = NULL,
    digit.remove = NULL, ...)

  termco.d(text.var, grouping.var = NULL, match.string,
    short.term = FALSE, ignore.case = TRUE,
    zero.replace = 0, output = "percent", digits = 2,
    apostrophe.remove = FALSE, char.keep = NULL,
    digit.remove = TRUE, ...)

  term.match(text.var, terms, return.list = TRUE,
    apostrophe.remove = FALSE)

  termco2mat(dataframe, drop.wc = TRUE, short.terms = TRUE,
    rm.zerocol = FALSE, no.quote = TRUE, transform = TRUE,
    trim.terms = TRUE)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{match.list}{a list of named character vectors}

  \item{short.term}{logical.  If TRUE column names are
  trimmed versions of the match list, other wise the terms
  are wrapped with 'term(phrase)'}

  \item{ignore.case}{logical.  If TRUE case is ignored.}

  \item{elim.old}{logical.  If TRUE eliminates the columns
  that are combined together by the named match.list.}

  \item{output}{Type of proportion output; either
  \code{"proportion"} (decimal format) or \code{"percent"}.
  Default is \code{"percent"}.}

  \item{digits}{integer indicating the number of decimal
  places (round) or significant digits (signif) to be used.
  Negative values are allowed.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophes from the text before examining.}

  \item{char.keep}{A character vector of symbol character
  (i.e. punctioation) that strip should keep.  The default
  is to strip everything except apostophes.}

  \item{digit.remove}{logical.  If TRUE strips digits from
  the text.}

  \item{\ldots}{Other argument supplied to strip.}

  \item{match.string}{A vector of terms to search for.
  When using inside of \code{term.match} the term(s) must
  be words or partial words but do not have to be when
  using \code{termco.d} (i.e. they can be phrases, symbols
  etc.).}

  \item{zero.replace}{Value to replace 0 values with.}

  \item{\code{term.match}}{- return.list logical.  If TRUE
  returns the output for multiple terms as a list by term
  rather than a vector.}

  \item{dataframe}{A termco.a (or termco.d) dataframe or
  object.}

  \item{drop.wc}{logical.  If TRUE the word count column
  will be dropped.}

  \item{short.colnames}{logical.  If TRUE the ``term()''
  portion of column names will be dropped.}

  \item{no.quote}{logical.  If TRUE the matrix will be
  printed without quotes if it's character.}

  \item{transform}{logical.  If TRUE the matrix will be
  transformed.}
}
\value{
  \code{termco.a} & \code{termco.d} - both return a list,
  of class "termco.d", of data frames and information
  regarding word counts: \item{raw}{raw word counts by
  grouping variable} \item{prop}{proportional word counts
  by grouping variable; proportional to each individual's
  word use} \item{rnp}{a character combination data frame
  of raw and proportional} \item{zero_replace}{value to
  replace zeros with; mostly internal use}
  \item{output}{character value for outpur type (either"
  "proportion" or "percent"; mostly internal use}
  \item{digits}{integer value od number of digits to
  display; mostly internal use}

  \code{term.match} - returns a list or vector of possible
  words that match term(s).

  \code{termco2mat} - returns a matrix of term counts.
}
\description{
  \code{termco.a} - Search a transcript by any number of
  grouping variables for categories (themes) of grouped
  root terms.  While there are other termco functions in
  the termco family (i.e. \code{termco.d}) \code{termco.a}
  is a wrapper for general use.

  \code{termco.d} - Search a transcript by any number of
  grouping variables for root terms.

  \code{term.match} - Search a transcript for words that
  exactly match term(s).

  \code{termco2mat} - Convert a termco dataframe to a
  matrix for use with visualization functions (e.g.
  heatmap2 of the gplots package).
}
\note{
  The match.list/match.string is (optionally) case and
  character sensitive.  Spacing is an important way to grab
  specific words and requires careful thought.  Using
  "read"will find the words "bread", "read" "reading", and
  "ready".  If you want to search fo just the word "read"
  you'd supply a vector of c(" read ", " reads", "
  reading", " reader").  To search for non character
  arguments (i.e. numbers and symbols) additional arguments
  from strip must be passed.
}
\examples{
\dontrun{
#termco.a examples:

# General form for match.list
#
# ml <- list(
#     cat1 = c(),
#     cat2 = c(),
#     catn = c()
# )

ml <- list(
    cat1 = c(" the ", " a ", " an "),
    cat2 = c(" I'" ),
    "good",
    the = c("the", " the ", " the", "the")
)

(dat <- with(raj.act.1,  termco.a(dialogue, person, ml)))
names(dat)
dat$rnp  #useful for presenting in tables
dat$raw  #prop and raw are useful for performing calculations
dat$prop
dat <- with(raj.act.1,  termco.a(dialogue, person, ml,
    short.term = FALSE, elim.old=FALSE))

dat2 <- data.frame(dialogue=c("@bryan is bryan good @br",
    "indeed", "@ brian"), person=qcv(A, B, A))

ml <- list(wrds=c("bryan", "indeed"), bryan=c("bryan", "@ br", "@br"))

with(dat2, termco.a(dialogue, person, match.list=ml, char.keep="@"))

with(dat2, termco.a(dialogue, person, match.list=ml,
    char.keep="@", output="proportion"))

DATA$state[1] <- "12 4 rgfr  r0ffrg0"
termco.a(DATA$state, DATA$person, '0', digit.remove=FALSE)

#Using with term.match and exclude
exclude(term.match(DATA$state, qcv(th), FALSE), "truth")
termco.a(DATA$state, DATA$person, exclude(term.match(DATA$state, qcv(th),
    FALSE), "truth"))
MTCH.LST <- exclude(term.match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco.a(DATA$state, DATA$person, MTCH.LST)

#termco.d examples:
term.match(DATA$state, qcv(i, the))
termco.d(DATA$state, DATA$person, c(" the", " i'"))
termco.d(DATA$state, DATA$person, c(" the", " i'"), ignore.case=FALSE)
termco.d(DATA$state, DATA$person, c(" the ", " i'"))

# termco2mat example:
MTCH.LST <- exclude(term.match(DATA$state, qcv(a, i)), qcv(is, it, am, shall))
termco_obj <- termco.a(DATA$state, DATA$person, MTCH.LST)
termco2mat(termco_obj)

# as a visual
dat <- termco2mat(termco_obj)
library(gplots)
heatmap.2(dat, trace="none")
}
}
\seealso{
  \code{\link[qdap]{termco_c}}
}
\keyword{word-search}

