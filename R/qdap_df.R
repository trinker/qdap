#' Create qdap Specific Data Structure
#' 
#' Creating this \pkg{qdap} specific data structure enables short hand with 
#' subsequent \pkg{qdap} function calls that utilize the \code{text.var} 
#' argument.  Combined with the \code{\link[qdap]{\%&\%}} operator, the user n
#' need not specify a data set or the \code{text.var} argument (as many 
#' \pkg{qdap} functions contain a \code{text.var} argument). 
#' 
#' @param dataframe A \code{\link[base]{data.frame}} with a text variable.  
#' Generally, \code{\link[qdap]{sentSplit}} should be run first 
#' (\code{\link[qdap]{sentSplit}} actually produces a 
#' \code{\link[base]{data.frame}} that is of the class \code{"qdap_df"}).
#' @param text.var The name of the \code{text.var} column.
#' @return Returns a \code{\link[base]{data.frame}} of the class \code{"qdap_df"}.
#' @references Inspired by \pkg{dplyr}'s \code{\link[dplyr]{tbl_df}} structure.
#' @keywords data structure
#' @rdname qdap_df
#' @export
#' @seealso \code{\link[qdap]{\%&\%}},
#' \code{\link[qdap]{sentSplit}}
#' @examples
#' \dontrun{
#' dat <- qdap_df(DATA, state)
#' dat %&% trans_cloud(grouping.var=person)
#' dat %&% trans_cloud(grouping.var=person, text.var=stemmer(DATA$state))
#' dat %&% termco(grouping.var=person, match.list=list("fun", "computer"))
#' class(dat)
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
#' 
#' ## Various examples with qdap functions
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
#' }
qdap_df <- function(dataframe, text.var) {
    stopifnot(is.data.frame(dataframe))
    class(dataframe) <- unique(c("qdap_df", class(dataframe)))

    mess <- suppressWarnings(try(text.var, silent = TRUE))
    if (inherits(mess, "try-error")) {
        text.var <- as.character(substitute(text.var))
    }

    ## coerce factor text to character
    dataframe[[text.var]] <- as.character(dataframe[[text.var]])

    if(is.mp(dataframe[[text.var]])) {
        warning(paste0("\nSome rows contain missing punctuation.", 
            "\nConsider further data cleaning or use of `add_incomplete`."))
    }   

    if (is.dp(dataframe[[text.var]])) {
        warning(paste0("\nSome rows contain double punctuation.", 
            "\nSuggested use of `sentSplit` function."))
    }

    attributes(dataframe)[["qdap_df_text.var"]] <- text.var
    dataframe
}



#' Change text.var column of a qdap_df Object
#' 
#' Change text.var column of a qdap_df object.
#' 
#' @param object A \code{\link[base]{data.frame}} of the class \code{"qdap_df"}.
#' @param value A character string of the updated \code{text.var} column.
#' @rdname qdap_df
#' @keywords Text
#' @export
Text <- function(object) {
    attributes(object)[["qdap_df_text.var"]] 
}

#' @rdname qdap_df
#' @export
"Text<-" <- function(object, value) {
    attributes(object)[["qdap_df_text.var"]] <- value
    object
}






