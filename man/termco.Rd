\name{termco}
\alias{term.match}
\alias{termco}
\alias{termco.d}
\alias{termco2mat}
\title{Search For and Count Terms}
\usage{
  termco(text.var, grouping.var = NULL, match.list,
    short.term = TRUE, ignore.case = TRUE, elim.old = TRUE,
    percent = TRUE, digits = 2, apostrophe.remove = FALSE,
    char.keep = NULL, digit.remove = NULL,
    zero.replace = 0, ...)

  termco.d(text.var, grouping.var = NULL, match.string,
    short.term = FALSE, ignore.case = TRUE,
    zero.replace = 0, percent = TRUE, digits = 2,
    apostrophe.remove = FALSE, char.keep = NULL,
    digit.remove = TRUE, ...)

  term.match(text.var, terms, return.list = TRUE,
    apostrophe.remove = FALSE)

  termco2mat(dataframe, drop.wc = TRUE, short.term = TRUE,
    rm.zerocol = FALSE, no.quote = TRUE, transform = TRUE,
    trim.terms = TRUE)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{match.list}{A list of named character vectors.}

  \item{short.term}{logical.  If TRUE column names are
  trimmed versions of the match list, other wise the terms
  are wrapped with 'term(phrase)'}

  \item{ignore.case}{logical.  If TRUE case is ignored.}

  \item{elim.old}{logical.  If TRUE eliminates the columns
  that are combined together by the named match.list.}

  \item{percent}{logical.  If TRUE output given as percent.
  If FALSE the output is proption.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophes from the text before examining.}

  \item{char.keep}{A character vector of symbol character
  (i.e. punctuation) that strip should keep.  The default
  is to strip everything except apostophes.
  \code{\link[qdap]{termco}} attempts to auto detect
  characters to keep based on the elements in
  \code{match.list}.}

  \item{digit.remove}{logical.  If TRUE strips digits from
  the text before counting. \code{\link[qdap]{termco}}
  attempts to auto detect if digits should be retained
  based on the elements in \code{match.list}.}

  \item{zero.replace}{Value to replace 0 values with.}

  \item{\ldots}{Other argument supplied to strip.}

  \item{match.string}{A vector of terms to search for.
  When using inside of \code{term.match} the term(s) must
  be words or partial words but do not have to be when
  using \code{termco.d} (i.e. they can be phrases, symbols
  etc.).}

  \item{terms}{The terms to serach for in the
  \code{text.var}.  Similar to \code{match.list} but these
  terms must be words or partial words rather than multiple
  words and symbols.}

  \item{return.list}{logical.  If TRUE returns the output
  for multiple terms as a list by term rather than a
  vector.}

  \item{dataframe}{A termco (or termco.d) dataframe or
  object.}

  \item{drop.wc}{logical.  If TRUE the word count column
  will be dropped.}

  \item{rm.zerocol}{logical.  If TRUE any column containing
  all zeros will be removed from the matrix.}

  \item{no.quote}{logical.  If TRUE the matrix will be
  printed without quotes if it's character.}

  \item{transform}{logical.  If TRUE the matrix will be
  transformed.}

  \item{trim.terms}{logical.  If TRUE trims the column
  header/names to ensure there is not a problem with
  spacing when using in other R functions.}
}
\value{
  \code{termco} & \code{termco.d} - both return a list, of
  class "termco.d", of data frames and information
  regarding word counts: \item{raw}{raw word counts by
  grouping variable} \item{prop}{proportional word counts
  by grouping variable; proportional to each individual's
  word use} \item{rnp}{a character combination data frame
  of raw and proportional} \item{zero_replace}{value to
  replace zeros with; mostly internal use}
  \item{output}{character value for outpur type (either"
  "proportion" or "percent"; mostly internal use}
  \item{digits}{integer value of number of digits to
  display; mostly internal use}

  \code{term.match} - returns a list or vector of possible
  words that match term(s).

  \code{termco2mat} - returns a matrix of term counts.
}
\description{
  \code{termco} - Search a transcript by any number of
  grouping variables for categories (themes) of grouped
  root terms.  While there are other termco functions in
  the termco family (i.e. \code{termco.d}) \code{termco} is
  a wrapper for general use.

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
\section{Warning}{
  Percentages are calulated as a ratio of counts of
  \code{match.list} elements to word counts.  Word counts
  do not contain symbols or digits.  Using symbols, digits
  or small segements of full words (e.g. "to") could total
  more than 100\%.
}
\examples{
\dontrun{
#termco examples:

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

(dat <- with(raj.act.1,  termco(dialogue, person, ml)))
names(dat)
dat$rnp  #useful for presenting in tables
dat$raw  #prop and raw are useful for performing calculations
dat$prop
dat <- with(raj.act.1,  termco(dialogue, person, ml,
    short.term = FALSE, elim.old=FALSE))

dat2 <- data.frame(dialogue=c("@bryan is bryan good @br",
    "indeed", "@ brian"), person=qcv(A, B, A))

ml <- list(wrds=c("bryan", "indeed"), "@", bryan=c("bryan", "@ br", "@br"))

with(dat2, termco(dialogue, person, match.list=ml))

with(dat2, termco(dialogue, person, match.list=ml, percent = FALSE))

DATA$state[1] <- "12 4 rgfr  r0ffrg0"
termco(DATA$state, DATA$person, '0', digit.remove=FALSE)
DATA <- qdap::DATA

#Using with term.match and exclude
exclude(term.match(DATA$state, qcv(th), FALSE), "truth")
termco(DATA$state, DATA$person, exclude(term.match(DATA$state, qcv(th),
    FALSE), "truth"))
MTCH.LST <- exclude(term.match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)

syns <- synonyms("doubt")
syns[1]
termco(DATA$state, DATA$person, unlist(syns[1]))
synonyms("doubt", FALSE)
termco(DATA$state, DATA$person, list(doubt = synonyms("doubt", FALSE)))
termco(DATA$state, DATA$person, syns)

#termco.d examples:
term.match(DATA$state, qcv(i, the))
termco.d(DATA$state, DATA$person, c(" the", " i'"))
termco.d(DATA$state, DATA$person, c(" the", " i'"), ignore.case=FALSE)
termco.d(DATA$state, DATA$person, c(" the ", " i'"))

# termco2mat example:
MTCH.LST <- exclude(term.match(DATA$state, qcv(a, i)), qcv(is, it, am, shall))
termco_obj <- termco(DATA$state, DATA$person, MTCH.LST)
termco2mat(termco_obj)
plot(termco_obj)
plot(termco_obj, label = TRUE)
plot(termco_obj, label = TRUE, text.color = "red")
plot(termco_obj, label = TRUE, text.color="red", lab.digits=3)
}
}
\seealso{
  \code{\link[qdap]{termco.c}}
}
\keyword{word-search}

