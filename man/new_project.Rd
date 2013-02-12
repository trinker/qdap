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
  \item{01_clean_data.R}{ * intial cleaning of raw
  transcripts} \item{02_analysis_I.R}{ * intial analysis}
  \item{03_plots.R}{ * plotting script} } } \item{CM_DATA}{
  - A directory to export/import scripts for cm_xxx family
  of functions} \item{CODEBOOK}{ - A directory to store
  coding conventions or demographics data}
  \item{CORRESPONDENCE}{ - A directory to store
  correspondence and agreements with the client}
  \item{DATA_CLEANED}{ - A directory to store cleaned data
  (generally .RData format)} \item{DATA_FOR_REVIEW}{ - A
  directory to put data that may need to be altered or
  needs to be inspected more closely} \item{DOCUMENTS}{ - A
  directory to store documents related tot he project}
  \item{PLOTS}{ - A directory to store plots}
  \item{RAW_TRANSCRIPTS}{ - A directory to store the raw
  transcripts} \item{REPORTS}{ - A directory to house
  reports} \item{TABLES}{ - A directory to export tables
  to} \item{WORD_LISTS}{ - A directory to store word lists
  that can be sourced and supplied to functions}
  \item{extra_functions.R}{ - A script to store user made
  functions related to the project} \item{LOG.txt}{ - A
  text file documenting project changes/needs etc.}
  \item{TO_DO.txt}{ - A text file documenting project
  tasks} }
}
\keyword{replace}

