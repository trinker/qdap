\name{new_project}
\alias{new_project}
\title{Project Template}
\usage{
  new_project(project = "new", path = getwd())
}
\arguments{
  \item{project}{A character vector of the project name.}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}
}
\value{
  Creates a project template.
}
\description{
  Generate a project template to increase efficiency.
}
\details{
  The project template includes these main directories and
  scripts: \itemize{ \item{ANALYSIS}{ - A directory
  containing the following analysis scripts: \itemize{
  \item{01_clean_data.R}{ * initial cleaning of raw
  transcripts} \item{02_analysis_I.R}{ * initial analysis}
  \item{03_plots.R}{ * plotting script} } } \item{CM_DATA}{
  - A directory to export/import scripts for cm_xxx family
  of functions} \item{CODEBOOK}{ - A directory to store
  coding conventions or demographics data \itemize{
  \item{KEY.csv}{ * A blank template for demographic
  information} } } \item{CORRESPONDENCE}{ - A directory to
  store correspondence and agreements with the client
  \itemize{ \item{CONTACT_INFO.txt}{ * A txt file to put
  research team members' contact information}
  \item{project_directions.pdf}{ * A pdf explaining the
  structure of the project template} } }
  \item{DATA_CLEANED}{ - A directory to store cleaned data
  (generally .RData format)} \item{DATA_FOR_REVIEW}{ - A
  directory to put data that may need to be altered or
  needs to be inspected more closely} \item{DOCUMENTS}{ - A
  directory to store documents related to the project}
  \item{PLOTS}{ - A directory to store plots}
  \item{RAW_TRANSCRIPTS}{ - A directory to store the raw
  transcripts} \item{REPORTS}{ - A directory to house
  reports; contains: \itemize{ \item{report_1.rnw}{ * A
  latex rnw file for use with
  \href{http://yihui.name/knitr/}{knitr}}
  \item{project.bib}{ * A latex bibtex file}
  \item{preamble.tex}{ * A tex file that
  \code{report_1.rnw} references to generate the preamble}
  } } \item{TABLES}{ - A directory to export tables to}
  \item{WORD_LISTS}{ - A directory to store word lists that
  can be sourced and supplied to functions}
  \item{extra_functions.R}{ - A script to store user made
  functions related to the project} \item{LOG.txt}{ - A
  text file documenting project changes/needs etc.}
  \item{TO_DO.txt}{ - A text file documenting project
  tasks} }
}
\keyword{replace}

