\name{repo2github}
\alias{repo2github}
\title{Upload a Local Repo to GitHub}
\usage{
repo2github(password, project.dir = getwd(), repo = basename(getwd()),
  github.user = getOption("github.user"), gitpath = NULL, readme = TRUE)
}
\arguments{
  \item{password}{GitHub user password (character string).
  If this is not supplied the user will be prompted to
  enter a password.}

  \item{project.dir}{The path to the root directory of the
  report/presentation.}

  \item{repo}{A character string naming the repo; default
  attempts to use the report project directory name.}

  \item{github.user}{GitHub user name (character string).}

  \item{gitpath}{Path to the location of git.  If
  \code{NULL} \code{repo2github} will attempt to locate the
  path if necessary.}

  \item{readme}{logical.  If \code{TRUE} repo initializes
  with a README.md file.}
}
\value{
Creates GitHub repository.
}
\description{
Allows uploading a local repository to GitHub without first
creating the repository in the clouds.
\code{\link[qdap]{repo2github}} is designed for the initial
push to github.  Future pushes can be handled via RStudio
or other git interface.
}
\details{
The arguments \code{project.dir} and \code{repo} use
\code{\link[base]{getwd}}.  This assumes is the current
working directoy is the root directory and is done for
convenience.  The user should ensure that either their
working directory is the root directory or supply the
correct root directory/name to these arguments.
}
\note{
The user will need to have a
\href{https://github.com/}{GitHub} account established.
}
\section{Warning}{
  For Windows users this function creates a temporary
  _netrc file in the home directory and attempts to delete
  this file.  The _netrc contains username and password
  information for github. \code{\link[qdap]{repo2github}}
  attempts to delete this file but care should be taken.
  The file is created in: \cr
  \code{file.path(Sys.getenv()["HOME"],
  "DELETE_ME_REPORTS_PACKAGE/_nectrc")}.
}

\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{github.user} in the user's primary
  \code{.Rprofile}.
}
\examples{
\dontrun{
repo2github()
}
}
\author{
Simon O'Hanlon, Daniel Chaffiol, and Tyler Rinker
<tyler.rinker@gmail.com>
}
\references{
\url{http://stackoverflow.com/a/15047013/1000343} \cr
\url{http://stackoverflow.com/a/18692400/1000343}
}

