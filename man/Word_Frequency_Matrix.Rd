\name{wfm}
\alias{wfdf}
\alias{wfm}
\alias{wfm_combine}
\alias{wfm_expanded}
\alias{wfm_weight}
\title{Word Frequency Matrix}
\usage{
wfm(text.var = NULL, grouping.var = NULL, output = "raw",
  stopwords = NULL, char2space = "~~", ...)

wfdf(text.var, grouping.var = NULL, stopwords = NULL, margins = FALSE,
  output = "raw", digits = 2, char2space = "~~", ...)

wfm_expanded(text.var, grouping.var = NULL, ...)

wfm_combine(wf.obj, word.lists, matrix = TRUE)

wfm_weight(wfm.obj, type = "prop")
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{output}{Output type (either \code{"proportion"} or
  \code{"percent"}).}

  \item{stopwords}{A vector of stop words to remove.}

  \item{char2space}{A vector of characters to be turned
  into spaces.  If \code{char.keep} is \code{NULL},
  \code{char2space} will activate this argument.}

  \item{\ldots}{Other arguments supplied to
  \code{\link[qdap]{strip}}.}

  \item{digits}{An integer indicating the number of decimal
  places (round) or significant digits (signif) to be used.
  Negative values are allowed.}

  \item{margins}{logical. If \code{TRUE} provides
  grouping.var and word variable totals.}

  \item{word.lists}{A list of character vectors of words to
  pass to \code{wfm_combine}}

  \item{matrix}{logical.  If \code{TRUE} returns the output
  as a \code{\link[qdap]{wfm}} rather than a
  \code{\link[qdap]{wfdf}} object.}

  \item{wf.obj}{A \code{wfm} or \code{wfdf} object.}

  \item{wfm.obj}{A \code{\link[qdap]{wfm}} object.}

  \item{type}{The type of weighting to use:
  c(\code{"prop"}, \code{"max"}, \code{"scaled"}).  All
  weight by column.  \code{"prop"} uses a proportion
  weighting and all columns sum to 1.  \code{"max"} weights
  in proportion to the max value; all values are integers
  and column sums may not be equal. \code{"scaled"} uses
  \code{\link[base]{scale}} to scale with \code{center =
  FALSE}; output is not integer and column sums may not be
  equal.}
}
\value{
\code{wfm} - returns a word frequency of the class matrix.

\code{wfdf} - returns a word frequency of the class
data.frame with a words column and optional margin sums.

\code{wfm_expanded} - returns a matrix similar to a word
frequency matrix (\code{wfm}) but the rows are expanded to
represent the maximum usages of the word and cells are
dummy coded to indicate that number of uses.

\code{wfm_combine} - returns a word frequency matrix
(\code{wfm}) or dataframe (\code{wfdf}) with counts for the
combined word.lists merged and remaining terms
(\code{else}).

\code{wfm_weight} - Returns a weighted matrix for use with
other R packages. The output is not of the class "wfm".
}
\description{
\code{wfm} - Generate a word frequency matrix by grouping
variable(s).

\code{wfdf} - Generate a word frequency data frame by
grouping variable.

\code{wfm_expanded} - Expand a word frequency matrix to
have multiple rows for each word.

\code{wfm_combine} - Combines words (rows) of a word
frequency matrix (\code{wfdf}) together.

\code{wfm_weight} - Weight a word frequency matrix for
analysis were such weighting is sensible..
}
\note{
Words can be kept as one by inserting a double tilde
(\code{"~~"}), or other character strings passed to
char2space, as a single word/entry. This is useful for
keeping proper names as a single unit.
}
\examples{
\dontrun{
## word frequency matrix (wfm) example:
with(DATA, wfm(state, list(sex, adult)))[1:15, ]
with(DATA, wfm(state, person))[1:15, ]
with(DATA, wfm(state, list(sex, adult)))

## insert double tilde ("~~") to keep phrases(i.e., first last name)
alts <- c(" fun", "I ")
state2 <- space_fill(DATA$state, alts, rm.extra = FALSE)
with(DATA, wfm(state2, list(sex, adult)))[1:18, ]

## word frequency dataframe (wfdf) example:
with(DATA, wfdf(state, list(sex, adult)))[1:15, ]
with(DATA, wfdf(state, person))[1:15, ]

## insert double tilde ("~~") to keep phrases (e.g., first last name)
alts <- c(" fun", "I ")
state2 <- mgsub(alts, gsub("\\\\s", "~~", alts), DATA$state)
with(DATA, wfdf(state2, list(sex, adult)))[1:18, ]

## wfm_expanded example:
z <- wfm(DATA$state, DATA$person)
wfm_expanded(z)[30:45, ] #two "you"s

## wf_combine examples:
#===================
## raw no margins (will work)
x <- wfm(DATA$state, DATA$person)

## raw with margin (will work)
y <- wfdf(DATA$state, DATA$person, margins = TRUE)

## Proportion matrix
z2 <- wfm(DATA$state, DATA$person, output="proportion")

WL1 <- c(y[, 1])
WL2 <- list(c("read", "the", "a"), c("you", "your", "you're"))
WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "you're"))
WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))
WL5 <- list(yous = c("you", "your", "your're"))
WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1
WL7 <- c("you", "your", "your're")

wfm_combine(z, WL2) #Won't work not a raw frequency matrix
wfm_combine(x, WL2) #Works (raw and no margins)
wfm_combine(y, WL2) #Works (raw with margins)
wfm_combine(y, c("you", "your", "your're"))
wfm_combine(y, WL1)
wfm_combine(y, WL3)
## wfm_combine(y, WL4) #Error
wfm_combine(y, WL5)
wfm_combine(y, WL6)
wfm_combine(y, WL7)

worlis <- c("you", "it", "it's", "no", "not", "we")
y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)
z <- wfm_combine(y, worlis)

chisq.test(z)
chisq.test(wfm(y))

## Words correlated within turns of talk
## EXAMPLE 1
library(reports)
x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
dat <- wfm(rajSPLIT$dialogue, x)

cor(t(dat)[, c("romeo", "juliet")])
cor(t(dat)[, c("romeo", "banished")])
cor(t(dat)[, c("romeo", "juliet", "hate", "love")])
qheat(cor(t(dat)[, c("romeo", "juliet", "hate", "love")]),
    diag.na = TRUE, values = TRUE, digits = 3, by.column = NULL)

dat2 <- wfm(DATA$state, seq_len(nrow(DATA)))
qheat(cor(t(dat2)), low = "yellow", high = "red",
    grid = "grey90", diag.na = TRUE, by.column = NULL)

## EXAMPLE 2
x2 <- factor(with(pres_debates2012, paste(time, pad(TOT(tot)), sep = "|")))
dat2 <- wfm(pres_debates2012$dialogue, x2)
wrds <- word_list(pres_debates2012$dialogue,
    stopwords = c("it's", "that's", Top200Words))
wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
qheat(word_cor(t(dat2), word = wrds2, r = NULL),
    diag.na = TRUE, values = TRUE, digits = 3, by.column = NULL,
    high="red", low="yellow", grid=NULL)

## EXAMPLE 3
library(gridExtra); library(ggplot2); library(grid)
dat3 <- lapply(qcv(OBAMA, ROMNEY), function(x) {
    with(pres_debates2012, wfm(dialogue[person == x], x2[person == x]))
})


# Presidential debates by person
dat5 <- pres_debates2012
dat5 <- dat5[dat5$person \%in\% qcv(ROMNEY, OBAMA), ]

disp <- with(dat5, dispersion_plot(dialogue, wrds2, grouping.var = person,
    total.color = NULL, rm.vars=time))


cors <- lapply(dat3, function(m) {
    word_cor(t(m), word = wrds2, r = NULL)
})

plots <- lapply(cors, function(x) {
    qheat(x, diag.na = TRUE, values = TRUE, digits = 3, plot = FALSE,
    by.column = NULL, high="red", low="yellow", grid=NULL)
})

plots <- lapply(1:2, function(i) {
    plots[[i]] + ggtitle(qcv(OBAMA, ROMNEY)[i]) +
    theme(axis.title.x = element_blank(),
        plot.margin = unit(rep(0, 4), "lines"))
})

grid.arrange(disp, arrangeGrob(plots[[1]], plots[[2]], ncol=1), ncol=2)

## With `word_cor`
worlis <- list(
    pronouns = c("you", "it", "it's", "we", "i'm", "i"),
    negative = qcv(no, dumb, distrust, not, stinks),
    literacy = qcv(computer, talking, telling)
)
y <- wfdf(DATA$state, id(DATA, prefix = TRUE))
z <- wfm_combine(y, worlis)

word_cor(t(z), word = names(worlis), r = NULL)

## Plotting method
plot(y, TRUE)
plot(z)

## Correspondence Analysis
library(ca)

dat <- pres_debates2012
dat <- dat[dat$person \%in\% qcv(ROMNEY, OBAMA), ]

speech <- stemmer(dat$dialogue)
mytable1 <- with(dat, wfm(speech, list(person, time), stopwords = Top25Words))

fit <- ca(mytable)
summary(fit)
plot(fit)
plot3d.ca(fit, labels=1)


mytable2 <- with(dat, wfm(speech, list(person, time), stopwords = Top200Words))

fit2 <- ca(mytable2)
summary(fit2)
plot(fit2)
plot3d.ca(fit2, labels=1)

## Weight a wfm
WFM <- with(DATA, wfm(state, list(sex, adult)))
plot(wfm_weight(WFM, "scaled"), TRUE)
wfm_weight(WFM, "prop")
wfm_weight(WFM, "max")
wfm_weight(WFM, "scaled")
}
}
\keyword{word-frequency-matrix}

