\name{summary.cmspans}
\alias{plot.sum_cmspans}
\alias{summary.cmspans}
\title{Summarize a cmspans object}
\usage{
  \method{summary}{cmspans} (object, grouping.var = NULL,
    rm.var = NULL, percent = TRUE, digits = 2, ...)

  \method{plot}{sum_cmspans} (x, digits = NULL, sep = ".",
    name.sep = "&", values = TRUE, high = "red",
    transform = TRUE, plot = TRUE, facet.vars = NULL, ...)
}
\arguments{
  \item{object}{The cmspans object}

  \item{grouping.var}{The grouping variables. Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{rm.var}{An optional single vector or list of 1 or 2
  of repeated measures to aggregate by.}

  \item{percent}{logical.  If \code{TRUE} output given as
  percent.  If \code{FALSE} the output is proportion.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}

  \item{\ldots}{Other argument passed to \code{qheat} in
  plot (ignored in summary).}

  \item{x}{The sum_cmspans object (the object produced by
  calling \code{summary} on a cmspans object)}

  \item{sep}{The character that was used in \code{paste2}
  to paste the columns.}

  \item{name.sep}{The character that was used to paste the
  column names.}

  \item{values}{logical.  If \code{TRUE} the cell values
  will be included on the heatmap.}

  \item{high}{The color to be used for higher values.}

  \item{transform}{logical.  If \code{TRUE} the dataframe
  is rotated 90 degrees.}

  \item{plot}{logical.  If \code{TRUE} the plot will
  automatically plot. The user may wish to set to
  \code{FALSE} for use in knitr, sweave, etc. to add
  additional plot layers.}

  \item{facet.vars}{A character vector of names to facet
  by.}
}
\description{
  \code{summary.cmspans} - Summarize a cmspans object

  \code{plot.sum_cmspans} - Plots a heat map of summary
  statistics for sum_cmspans objects (the object produced
  by calling \code{summary} on a cmspans object).
}
\examples{
\dontrun{
## Example 1
foo <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="1"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:9, 100:150")
)

foo2  <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

v <- cm_2long(foo, foo2, v.name = "time")
plot(v)
summary(v)
plot(summary(v))
plot(summary(v), facet.vars = "time")

## Example 2
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
        9.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)
z <-cm_2long(x)

summary(z)
plot(summary(z))

## suppress printing measurement units
suppressMessages(print(summary(z)))

## remove print method
z_unclass <- summary(z)
class(z_unclass) <- "data.frame"
z_unclass
}
}

