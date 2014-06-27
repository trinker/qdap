#' qdap Chaining
#' 
#' Chain \code{\link[qdap]{qdap_df}}s to \pkg{qdap} functions with a 
#' \code{text.var} argument.  Saves typing of an explicit \code{text.var} 
#' argument and supplying a \code{\link[base]{data.frame}}. 
#' 
#' @param qdap_df.object A \code{\link[base]{data.frame}} of the class 
#' \code{"qdap_df"}.
#' @param qdap.fun A \pkg{qdap} function with a \code{text.var} argument.
#' @references Inspired by \pkg{dplyr}'s \code{\link[dplyr]{\%.\%}} and 
#' \pkg{magrittr}'s \code{\link[dplyr]{\%>\%}} functionality.
#' @keywords pipe chain chaining
#' @seealso \code{\link[dplyr]{\%>\%}},
#' \code{\link[qdap]{qdap_df}}
#' @export
#' @rdname chain
#' @examples
#' \dontrun{
#' dat <- qdap_df(DATA, state)
#' dat %&% trans_cloud(grouping.var=person)
#' dat %&% trans_cloud(grouping.var=person, text.var=stemmer(DATA$state))
#' dat %&% termco(grouping.var=person, match.list=list("fun", "computer"))
#' 
#' dat <- sentSplit(DATA, "state", stem.col = TRUE)
#' dat %&% trans_cloud(grouping.var=person)
#' dat %&% termco(person, match.list=list("fun", "computer"))
#' dat %&% trans_venn(person)
#' dat %&% polarity(person)
#' dat %&% formality(person)
#' dat %&% automated_readability_index(person)
#' dat %&% Dissimilarity(person)
#' dat %&% gradient_cloud(sex)
#' dat %&% dispersion_plot(c("fun", "computer"))
#' dat %&% discourse_map(list(sex, adult))
#' dat %&% gantt_plot(person)
#' dat %&% word_list(adult)
#' dat %&% end_mark_by(person)
#' dat %&% end_mark()
#' dat %&% word_stats(person)
#' dat %&% wfm(person)
#' dat %&% word_cor(person, "i")
#' dat %&% sentCombine(person)
#' dat %&% question_type(person)
#' 
#' ## combine with magrittr/dplyr
#' library(magrittr)
#' dat %&% wfm(person) %>% plot()
#' dat %&% polarity(person) %>% scores()
#' dat %&% polarity(person) %>% counts()
#' dat %&% polarity(person) %,% scores()
#' 
#' ## change text column in `qdap_df`
#' dat %&% trans_cloud()
#' Text(dat)
#' Text(dat) <- "stem.text"
#' dev.new()
#' dat %&% trans_cloud() # notice words are stemmed
#' }
`%&%` <- function(qdap_df.object, qdap.fun) {

    stopifnot(inherits(qdap_df.object, "qdap_df"))

    thecall <- substitute(qdap.fun)

    if(is.null(thecall$text.var)) {
        thecall$text.var <- as.name(attributes(qdap_df.object)[["qdap_df_text.var"]])
    }

    eval(thecall, qdap_df.object, parent.frame())
}


