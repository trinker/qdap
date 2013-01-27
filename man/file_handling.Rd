\name{delete}
\alias{delete}
\alias{folder}
\title{Easy File Handling}
\usage{
  delete(file = NULL)

  folder(..., folder.name = NULL)
}
\arguments{
  \item{file}{The name of the file in the working directory
  or the path to the file to be deleted.  If NULL provides
  a menu of files from the working directory.}

  \item{\ldots}{The name(s) of the folder to be created.
  If both \ldots and \code{folder.name} are NULL creates a
  file in the working directory with the creation date and
  time stamp.}

  \item{folder.name}{The name(s) of the folder to be
  created.  Default NULL (if \ldots is NULL too) creates a
  file in the working directory with the creation date and
  time stamp.  Use this argument only if the directory
  names have spaces within them.}
}
\value{
  \code{delete} permanently removes a file/directory.

  \code{folder} creates a folder/directory.
}
\description{
  \code{delete} - Deletes files and directories.

  \code{folder} - Create a folder/directory.
}
\examples{
\dontrun{
##  (x <- folder("DELETE.ME"))
##  which(dir() == "DELETE.ME")
##  delete("DELETE.ME")
##  which(dir() == "DELETE.ME")
##
##  folder(cat, dog)
##  lapply(c("cat", "dog"), delete)
}
}
\seealso{
  \code{\link[base]{unlink}},
  \code{\link[base]{file.remove}},
  \code{\link[base]{dir.create}}
}
\keyword{delete,}
\keyword{file,}
\keyword{folder}

