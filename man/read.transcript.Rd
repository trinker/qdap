\name{read.transcript}
\alias{read.transcript}
\title{Read Transcripts Into R}
\usage{
  read.transcript(file, col.names = NULL, text.var = NULL,
    merge.broke.tot = TRUE, header = FALSE, dash = "",
    ellipsis = "...", quote2bracket = FALSE,
    rm.empty.rows = TRUE,
    na.strings = c("999", "NA", "", " "), sep = NULL,
    skip = 0, nontext2factor = TRUE, ...)
}
\arguments{
  \item{file}{The name of the file which the data are to be
  read from. Each row of the table appears as one line of
  the file. If it does not contain an absolute path, the
  file name is relative to the current working directory,
  getwd().}

  \item{col.names}{Supplies a vector of column names to the
  transcript columns.}

  \item{text.var}{specifying the name of the text variable
  will ensure that variable is classed as character.  If
  NULL read.transcript attempts to guess the text.variable
  (dialogue).}

  \item{merge.broke.tot}{If the file being read in is .docx
  and the transcript if formated to have broken space
  between a single turn of talk read.transcript will
  attempt to merge these into a single turn of talk.}

  \item{header}{logical.  If TRUE the file contains the
  names of the variables as its first line.}

  \item{dash}{Character to replace the en and em dashes
  special characters (default is to remove).}

  \item{ellipsis}{Character to replace the ellipsis special
  characters (default is text ...).}

  \item{quote2bracket}{logical If TRUE replaces curly
  quotes with curly braces (default is FALSE).  If FALSE
  curly quotes are removed.}

  \item{rm.empty.rows}{logical.  If TURE read.transcript
  attempts to remove empty rows.}

  \item{na.strings}{A character vector of strings which are
  to be interpreted as NA values.}

  \item{sep}{The field separator character. Values on each
  line of the file are separated by this character.  The
  default of NULL instructs read.transcript to use a
  separator suitable for the file type being read in.}

  \item{skip}{Integer; the number of lines of the data file
  to skip before beginning to read data.}

  \item{nontext2factor}{logical.  If TRUE attempts to
  convert any non text to a factor.}

  \item{\ldots}{Further arguments to be passed to
  read.table.}
}
\value{
  Returns a dataframe of dialogue and people.
}
\description{
  Read a .docx, .csv or .xlsx files into R.
}
\note{
  If a transcript is a .docx file read transcript expects
  two columns (generally person and dialogue) with some
  sort of separator (default is colon separator).  .doc
  fils must be converted to .docx before reding in.
}
\examples{
\dontrun{
doc1 <- system.file("extdata/trans1.docx", package = "qdap")
doc2 <- system.file("extdata/trans2.docx", package = "qdap")
doc3 <- system.file("extdata/trans3.docx", package = "qdap")
doc4 <- system.file("extdata/trans4.xlsx", package = "qdap")

read.transcript(doc1)
dat <- read.transcript(doc1, col.names = c("person", "dialogue"))
dat
rm_row(dat, "person", "[C") #remove bracket row

read.transcript(doc2) #throws an error
read.transcript(doc2, skip = 1)
read.transcript(doc3, skip = 1) #wrong sep
read.transcript(doc3, sep = "-", skip = 1)
read.transcript(doc4)
}
}
\author{
  Bryan Goodrich and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
  \url{https://github.com/trinker/qdap/wiki/Reading-Transcripts-into-R}
}
\keyword{transcript}

