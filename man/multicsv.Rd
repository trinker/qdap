\name{mcsv_r}
\alias{mcsv_r}
\alias{mcsv_w}
\title{Read/Write Multiple csv Files at a Time}
\usage{
  mcsv_r(files, a.names = NULL, l.name = NULL, list = TRUE,
    pos = 1, envir = as.environment(pos))

  mcsv_w(..., dir = NULL, open = FALSE)
}
\arguments{
  \item{files}{csv file(s) to read.}

  \item{a.names}{object names to assign the csv file(s) to.
  If NULL assigns the csv to the name(s) of the csv file(s)
  in the global environment.}

  \item{l.name}{A character vector of names to assign to
  the csv files (dataframes) being read in.  Default (NULL)
  uses the names of the files in the directory without the
  file extension.}

  \item{list}{A character vector of length one to name the
  list being read in. Default is \code{"L1"}.}

  \item{pos}{where to do the removal. By default, uses the
  current environment.}

  \item{envir}{the environment to use.}

  \item{\dots}{data.frame object(s) to write to a file}

  \item{dir}{optional directory names.  If NULL a directory
  will be created in the working directory with the data
  and time stamp as the folder name.}

  \item{open}{logical.  If TURE opens the directory upon
  completion.}
}
\value{
  \code{mcsv_r} - reads in multiple csv files at once.

  \code{mcsv_w} - creates a directory with multiple csv
  files. Silently returns the path of the directory.
}
\description{
  \code{mcsv_w} - Read and assign multiple csv files at the
  same time.

  \code{mcsv_w} - Write multiple csv files into a file at
  the same time.
}
\details{
  mcsv is short for "multiple csv" and the suffix c(_r, _w)
  stands for "read" (r) or "write" (w).
}
\note{
  \code{mcsv_r} is useful for reading in multiple csv files
  from \code{cm_csv.temp} for interaction with
  \code{cm_range2long}.
}
\examples{
\dontrun{
#mcsv_r EXAMPLE:
mtcarsb <- mtcars[1:5, ]; CO2b <- CO2[1:5, ]
(a <- mcsv_w(mtcarsb, CO2b, dir="foo"))
rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
(nms <- dir(a))
mcsv_r(paste(a, nms, sep="/"))
mtcarsb; CO2b
rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
mcsv_r(paste(a, nms, sep="/"), paste0("foo.dat", 1:2))
foo.dat1; foo.dat2
rm("foo.dat1", "foo.dat2")  # gone from .GlobalEnv
delete("foo")

#mcsv_w EXAMPLE:
(a <- mcsv_w(mtcars, CO2, dir="foo"))
delete("foo")
}
}
\seealso{
  \code{\link[qdap]{cm_range2long}},
  \code{\link[qdap]{cm_df.temp}},
  \code{\link[base]{assign}}
}

