\name{apply_as_df}
\alias{apply_as_df}
\title{Apply a tm Corpus as a qdap Dataframe}
\usage{
apply_as_df(tm.corpus, qdapfun, ...)
}
\arguments{
  \item{tm.corpus}{A \pkg{tm} \code{\link[tm]{Corpus}}.}

  \item{qdapfun}{A qdap function that is usually used on
  text.variable ~ grouping variable.}
}
\description{
Apply a \pkg{tm} \code{\link[tm]{Corpus}} as a qdap
dataframe.
}
\note{
\code{aply_ad_df} coherses to a dataframe with columns
named `docs` and the othe rnames `docs`.
}
\examples{
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),
    readerControl = list(reader = readReut21578XML))

apply_as_df(reuters, word_stats)
apply_as_df(reuters, formality)
apply_as_df(reuters, word_list)
apply_as_df(reuters, polarity)
apply_as_df(reuters, Dissimilarity)
apply_as_df(reuters, diversity)
apply_as_df(reuters, pos_by)
apply_as_df(reuters, flesch_kincaid)
apply_as_df(reuters, trans_venn)
apply_as_df(reuters, gantt_plot)
apply_as_df(reuters, rank_freq_mplot)
apply_as_df(reuters, termco,
    match.list = list(
        oil = qcv(oil, Texas, crude),
        money = c("economic", "money")
    ))
plot(apply_as_df(reuters, termco,
    match.list = list(
        oil = qcv(oil, Texas, crude),
        money = c("economic", "money")
    ), elim.old = FALSE), values = TRUE, high="red")
apply_as_df(reuters, word_cor,
    word = qcv(oil, Texas, crude, economic, money)
)
plot(apply_as_df(reuters, word_cor,
    word = qcv(oil, Texas, crude, economic, money)
))
}
\seealso{
\code{\link[tm]{Corpus}}
}

