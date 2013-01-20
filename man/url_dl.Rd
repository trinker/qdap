\name{url_dl}
\alias{url_dl}
\title{Download Instructional Documents}
\usage{
  url_dl(..., url = "http://dl.dropbox.com/u/61803503/")
}
\arguments{
  \item{\ldots}{Document names to download.}

  \item{url}{The download url.}
}
\value{
  Places a copy of the downloaded document in the users
  working directory.
}
\description{
  This function enables downloading documents for future
  instructional training.
}
\note{
  Not intended for general use.
}
\examples{
\dontrun{
# download transcript of the debate to working directory
url_dl(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)

# load multiple files with read transcript and assign to working directory
dat1 <- read.transcript("pres.deb1.docx", c("person", "dialogue"))
dat2 <- read.transcript("pres.deb2.docx", c("person", "dialogue"))
dat3 <- read.transcript("pres.deb3.docx", c("person", "dialogue"))

docs <- qcv(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)
dir() \%in\% docs
delete(docs)    #remove the documents
dir() \%in\% docs
}
}

