\name{dispersion_plot}
\alias{dispersion_plot}
\title{Lexical Dispersion Plot}
\usage{
  dispersion_plot(text.var, match.terms,
    grouping.var = NULL, rm.vars = NULL, color = "blue",
    bg.color = "grey90", horiz.color = "grey85",
    symbol = "|", title = "Lexical Dispersion Plot",
    rev.factor = TRUE, wrap = "'",
    xlab = "Dialogue (Words)", ylab = NULL, size = 4,
    plot = TRUE, char2space = "~~", scales = "free",
    space = "free", total.color = "black")
}
\arguments{
  \item{text.var}{The text variable.}

  \item{match.terms}{A vector of quoted terms.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{rm.vars}{The repeated measures variables.  Default
  \code{NULL} generates one facet for all text.  Also takes
  a single repeated measures variable or a list of 1 or
  more grouping variables.}

  \item{color}{The color of the word symbols.}

  \item{bg.color}{The background color.}

  \item{horiz.color}{The color of the horizontal tracking
  stripe.  Use \code{horiz.color = bg.color} to eliminate.}

  \item{symbol}{The word symbol.  Defualt is \code{"|"}.}

  \item{title}{Title of the plot}

  \item{rev.factor}{logical.  If \code{TRUE} reverses the
  plot order of the factors.}

  \item{wrap}{a character to wrap around the words (enables
  the reader to visualize spaces).  Defualt is \code{"'"},
  use \code{""} to remove.}

  \item{xlab}{The x label.}

  \item{ylab}{The y label.}

  \item{size}{The size of the plotting symbol.}

  \item{plot}{logical.  If \code{TRUE} the plot will
  automatically plot. The user may wish to set to
  \code{FALSE} for use in knitr, sweave, etc. to add
  additional plot layers.}

  \item{char2space}{A vector of characters to be turned
  into spaces.}

  \item{scales}{Should scales be fixed (\code{"fixed"}, the
  default), free (\code{"free"}), or free in one dimension
  (\code{"free_x"}, \code{"free_y"})}

  \item{space}{If \code{"fixed"}, the default, all panels
  have the same size. If \code{"free_y"} their height will
  be proportional to the length of the y scale; if
  \code{"free_x"} their width will be proportional to the
  length of the x scale; or if \code{"free"} both height
  and width will vary.}

  \item{total.color}{The color to use for summary `all`
  group.  If \code{NULL} totals are dropped.}
}
\value{
  Plots a dispersion plot and invisibly returns the ggplot2
  object.
}
\description{
  Generate a lexical dispersion plot of terms.
}
\note{
  The match.terms is character sensitive.  Spacing is an
  important way to grab specific words and requires careful
  thought.  Using "read" will find the words "bread",
  "read" "reading", and "ready".  If you want to search for
  just the word "read" you'd supply a vector of c(" read ",
  " reads", " reading", " reader").
}
\examples{
\dontrun{
term_match(raj$dialogue, c(" love ", "love", " night ", "night"))
dispersion_plot(raj$dialogue, c(" love ", "love", " night ", "night"))
dispersion_plot(raj$dialogue, c("love", "night"), rm.vars = raj$act)
with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"),
    grouping.var = list(fam.aff, sex), rm.vars = act))

## With grouping variables
with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"),
     grouping.var = sex, rm.vars = act))

## Drop total with `total.color = NULL`
with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"),
     grouping.var = sex, rm.vars = act, total.color = NULL))

## Change color scheme
with(rajSPLIT, dispersion_plot(dialogue, c("love", "night"),
    bg.color = "black", grouping.var = list(fam.aff, sex),
    color = "yellow", total.color = "white", horiz.color="grey20"))

## Use word list
wrds <- word_list(pres_debates2012$dialogue, stopwords = Top200Words)
wrds2 <- spaste(wrds[["rfswl"]][["all"]][, "WORD"])
wrds2 <- c(" governor~~romney ", wrds2[-c(3, 12)])
with(pres_debates2012 , dispersion_plot(dialogue, wrds2, rm.vars = time))
}
}
\seealso{
  \code{\link[qdap]{term_match}}
}
\keyword{dispersion}

