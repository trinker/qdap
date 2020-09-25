#' qdap Chaining
#' 
#' \code{\%&\%} - Chain \code{\link[qdap]{qdap_df}}s to \pkg{qdap} functions with a 
#' \code{text.var} argument.  Saves typing of an explicit \code{text.var} 
#' argument and supplying a \code{\link[base]{data.frame}}. 
#' 
#' @param qdap_df.object A \code{\link[base]{data.frame}} of the class 
#' \code{"qdap_df"}.
#' @param qdap.fun A \pkg{qdap} function with a \code{text.var} argument.
#' @references Inspired by \pkg{magrittr}'s \code{\link[dplyr]{\%>\%}} functionality.
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
#' ## Various examples with qdap functions (sentSplit gives class "qdap_df")
#' dat <- sentSplit(DATA, "state")
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
#' dat %&% word_network_plot()
#' dat %&% character_count()
#' dat %&% char_table(person)
#' dat %&% phrase_net(2, .1)
#' dat %&% boolean_search("it||!")
#' dat %&% trans_context(person, which(end_mark(DATA.SPLIT[, "state"]) == "?"))
#' dat %&% mgsub(c("it's", "I'm"), c("it is", "I am"))
#' 
#' ## combine with magrittr/dplyr chaining
#' dat %&% wfm(person) %>% plot()
#' dat %&% polarity(person) %>% scores()
#' dat %&% polarity(person) %>% counts()
#' dat %&% polarity(person) %>% scores()
#' dat %&% polarity(person) %>% scores() %>% plot()
#' dat %&% polarity(person) %>% scores %>% plot
#' 
#' ## Change text column in `qdap_df` (Example 1)
#' dat2 <- sentSplit(DATA, "state", stem.col = TRUE)
#' class(dat2)
#' dat2 %&% trans_cloud()
#' Text(dat2)
#' ## change the `text.var` column
#' Text(dat2) <- "stem.text"
#' dat2 %&% trans_cloud()
#' 
#' ## Change text column in `qdap_df` (Example 2)
#' (dat2$fake_dat <- paste(emoticon[1:11,2], dat2$state))
#' Text(dat2) <- "fake_dat"
#' (m <- dat2 %&% sub_holder(emoticon[,2]))
#' m$unhold(strip(m$output))
#' }
`%&%` <- function(qdap_df.object, qdap.fun) {

    stopifnot(inherits(qdap_df.object, "qdap_df"))

    thecall <- substitute(qdap.fun)

    the_fun <- as.list(thecall)[[1]]
    if(!"text.var" %in% names(formals(match.fun(the_fun)))) {
        stop(sprintf("%s does not have `text.var` as a formal argument", 
            as.character(the_fun)))
    }

    if(is.null(thecall$text.var)) {
        thecall$text.var <- as.name(attributes(qdap_df.object)[["qdap_df_text.var"]])
    }

    eval(thecall, qdap_df.object, parent.frame())
}



#' qdap Chaining
#' 
#' \code{\%>\%} - The \pkg{magrittr} "then" chain operator imported by 
#' \pkg{dplyr}.  Imported for convenience.  See 
#' https://github.com/tidyverse/magrittr for details.
#' 
#' @param lhs The value to be piped.
#' @param rhs A function or expression.
#' @export
#' @importFrom dplyr tbl_df
#' @rdname chain
`%>%` <- dplyr::`%>%`
