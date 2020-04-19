#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.tdm} - Create term document matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#'
#' @param text.var The text variable or a \code{\link[qdap]{wfm}} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots Function dependant:
#' \itemize{
#'   \item \bold{as.tdm} or \bold{as.dtm} - Other arguments passed to \code{wfm}
#'   \item \bold{apply_as_tm} - Other arguments passed to functions used on a \pkg{tm} \code{TermDocumentMatrix}
#'   \item \bold{as.data.frame} - Other arguments passed to \code{\link[qdap]{sentSplit}}
#'   \item \bold{as.Corpus} - Other arguments passed to \pkg{tm}'s \code{\link[tm]{Corpus}}
#' }
#' @param vowel.check logical.  Should terms without vowels be remove?  
#' @details Produces output that is identical to the \code{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}, \code{\link[tm]{DocumentTermMatrix}},
#' \code{\link[tm]{Corpus}} or allows convenient interface between the qdap and 
#' tm packages.
#' @return \code{as.tdm} - Returns a \code{\link[tm]{TermDocumentMatrix}}.
#' @export
#' @seealso \code{\link[tm]{DocumentTermMatrix}},
#' \code{\link[tm]{Corpus}},
#' \code{\link[tm]{TermDocumentMatrix}},
#' \code{\link[qdap]{as.wfm}}
#' @importFrom reshape2 melt
#' @importFrom tm tm_map PlainTextDocument VectorSource Corpus
#' @rdname as.tdm
#' @examples
#' \dontrun{
#' as.dtm(DATA$state, DATA$person)
#' as.tdm(DATA$state, DATA$person)
#' 
#' x <- wfm(DATA$state, DATA$person)
#' as.tdm(x)
#' as.dtm(x)
#' library(tm)
#' plot(as.tdm(x))
#' 
#' pres <- as.tdm(pres_debates2012$dialogue, pres_debates2012$person)
#' plot(pres, corThreshold = 0.8)
#' pres
#' (pres2 <- removeSparseTerms(pres, .3))
#' plot(pres2, corThreshold = 0.95)
#' 
#' shorts <- all_words(pres_debates2012)[,1][nchar(all_words(
#'     pres_debates2012)[,1]) < 4]
#' 
#' SW <- c(shorts, qdapDictionaries::contractions[, 1],
#'     qdapDictionaries::Top200Words,
#'     "governor", "president", "mister", "obama","romney")
#' 
#' DocTermMat2 <- with(pres_debates2012, as.dtm(dialogue, list(person, time), stopwords = SW))
#' DocTermMat2 <- removeSparseTerms(DocTermMat2,0.95)
#' (DocTermMat2 <- DocTermMat2[rowSums(as.matrix(DocTermMat2))> 0,])
#' plot(DocTermMat2)
#'     
#' ## Correspondence Analysis
#' library(ca)
#' 
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' speech <- stemmer(dat$dialogue)
#' mytable1 <- with(dat, as.tdm(speech, list(person, time), stopwords = Top25Words))
#' 
#' fit <- ca(as.matrix(mytable1))
#' summary(fit)
#' plot(fit)
#' plot3d.ca(fit, labels=1)
#' 
#' 
#' mytable2 <- with(dat, as.tdm(speech, list(person, time), stopwords = Top200Words))
#' 
#' fit2 <- ca(as.matrix(mytable2))
#' summary(fit2)
#' plot(fit2)
#' plot3d.ca(fit2, labels=1)
#' 
#' ## Topic Models
#' # Example 1 #
#' library(topicmodels); library(tm)
#' 
#' # Generate stop words based on short words, frequent words and contractions
#' shorts <- all_words(pres_debates2012)[,1][nchar(all_words(
#'     pres_debates2012)[,1]) < 4]
#'     
#' SW <- c(shorts, qdapDictionaries::contractions[, 1], 
#'     qdapDictionaries::Top200Words, 
#'     "governor", "president", "mister", "obama","romney")
#'     
#' DocTermMat <- with(pres_debates2012, as.dtm(dialogue, person, stopwords = SW))
#' DocTermMat <- removeSparseTerms(DocTermMat,0.999)
#' DocTermMat <- DocTermMat[rowSums(as.matrix(DocTermMat))> 0,]
#' 
#' lda.model <- LDA(DocTermMat, 5)
#' 
#' (topics <- posterior(lda.model, DocTermMat)$topics)
#' terms(lda.model,20)
#' 
#' # Plot the Topics Per Person
#' topic.dat <- matrix2df(topics, "Person")
#' colnames(topic.dat)[-1] <- paste2(t(terms(lda.model,20)), sep=", ")
#' 
#' library(reshape2)
#' mtopic <- melt(topic.dat, variable="Topic", value.name="Proportion")
#' ggplot(mtopic, aes(weight=Proportion, x=Topic, fill=Topic)) + 
#'     geom_bar() + 
#'     coord_flip() +
#'     facet_grid(Person~.) +
#'     guides(fill=FALSE)
#' 
#' # Example 2 #
#' DocTermMat2 <- with(pres_debates2012, as.dtm(dialogue, list(person, time), stopwords = SW))
#' DocTermMat2 <- removeSparseTerms(DocTermMat2,0.95)
#' DocTermMat2 <- DocTermMat2[rowSums(as.matrix(DocTermMat2))> 0,]
#' 
#' lda.model2 <- LDA(DocTermMat2, 6)
#' 
#' (topics2 <- posterior(lda.model2, DocTermMat2)$topics)
#' terms(lda.model2,20)
#' qheat(topics2, high="blue", low="yellow", by.col=FALSE)
#' 
#' # Example 3 #
#' lda.model3 <- LDA(DocTermMat2, 10)
#' 
#' (topics3 <- posterior(lda.model3, DocTermMat2)$topics)
#' terms(lda.model3, 20)
#' qheat(topics3, high="blue", low="yellow", by.col=FALSE)
#' 
#' # Plot the Topics Per Person
#' topic.dat3 <- matrix2df(topics3, "Person&Time")
#' colnames(topic.dat3)[-1] <- paste2(t(terms(lda.model3, 10)), sep=", ")
#' topic.dat3 <- colsplit2df(topic.dat3)
#' 
#' library(reshape2)
#' library(scales)
#' mtopic3 <- melt(topic.dat3, variable="Topic", value.name="Proportion")
#' (p1 <- ggplot(mtopic3, aes(weight=Proportion, x=Topic, fill=Topic)) +
#'     geom_bar() +
#'     coord_flip() +
#'     facet_grid(Person~Time) +
#'     guides(fill=FALSE) +
#'     scale_y_continuous(labels = percent) +
#'     theme(plot.margin = unit(c(1, 0, 0.5, .5), "lines")) +
#'     ylab("Proportion")) 
#' 
#' mtopic3.b <- mtopic3
#' mtopic3.b[, "Topic"] <- factor(as.numeric(mtopic3.b[, "Topic"]), levels = 1:10)
#' mtopic3.b[, "Time"] <- factor(gsub("time ", "", mtopic3.b[, "Time"]))
#' 
#' p2 <- ggplot(mtopic3.b, aes(x=Time, y=Topic, fill=Proportion)) +
#'     geom_tile(color = "white") +
#'     scale_fill_gradient(low = "grey70", high = "red") +
#'     facet_grid(Person~Time, scales = "free") +
#'     theme(axis.title.y = element_blank(),
#'         axis.text.x= element_text(colour="white"),
#'         axis.ticks.x= element_line(colour="white"),
#'         axis.ticks.y = element_blank(),
#'         axis.text.y= element_blank(),
#'         plot.margin = unit(c(1, -.5, .5, -.9), "lines")
#' ) 
#' 
#' library(gridExtra)
#' grid.arrange(p1, p2, nrow=1, widths = grid::unit(c(.85, .15), "native")) 
#'     
#' ## tm Matrices to wfm
#' library(tm)
#' data(crude)
#'
#' ## A Term Document Matrix Conversion
#' (tm_in <- TermDocumentMatrix(crude, control = list(stopwords = TRUE)))
#' converted <- as.wfm(tm_in)
#' head(converted)
#' summary(converted)
#'
#' ## A Document Term Matrix Conversion
#' (dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
#' summary(as.wfm(dtm_in))
#' 
#' ## `apply_as_tm` Examples
#' ## Create a wfm
#' a <- with(DATA, wfm(state, list(sex, adult)))
#' summary(a)
#' 
#' ## Apply functions meant for a tm TermDocumentMatrix
#' out <- apply_as_tm(a, tm:::removeSparseTerms, sparse=0.6)
#' summary(out)
#' 
#' apply_as_tm(a, tm:::findAssocs, "computer", .8)
#' apply_as_tm(a, tm:::findFreqTerms, 2, 3)
#' apply_as_tm(a, tm:::Zipf_plot)
#' apply_as_tm(a, tm:::Heaps_plot)
#' apply_as_tm(a, tm:::plot.TermDocumentMatrix, corThreshold = 0.4)
#' 
#' library(proxy)
#' apply_as_tm(a, tm:::weightBin)
#' apply_as_tm(a, tm:::weightBin, to.qdap = FALSE)
#' apply_as_tm(a, tm:::weightSMART)
#' apply_as_tm(a, tm:::weightTfIdf)
#' 
#' ## Convert tm Corpus to Dataframe
#' ## A tm Corpus
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters <- Corpus(DirSource(reut21578),
#'     readerControl = list(reader = readReut21578XML))
#' 
#' ## Convert to dataframe
#' corp_df <- as.data.frame(reuters)
#' htruncdf(corp_df)
#' 
#' z <- as.Corpus(DATA$state, DATA$person, 
#'        demographic=DATA[, qcv(sex, adult, code)])
#' as.data.frame(z)
#' 
#' ## Apply a qdap function
#' out <- formality(corp_df$text, corp_df$docs)
#' plot(out)
#' 
#' ## Convert a qdap dataframe to tm package Corpus
#' (x <- with(DATA2, as.Corpus(state, list(person, class, day))))
#' library(tm)
#' inspect(x)
#' inspect_text(x)
#' class(x)
#' 
#' (y <- with(pres_debates2012, as.Corpus(dialogue, list(person, time))))
#' 
#' ## Add demographic info to DMetaData of Corpus
#' z <- as.Corpus(DATA$state, DATA$person, 
#'     demographic=DATA[, qcv(sex, adult, code)])
#' lview(z)
#' 
#' lview(as.Corpus(DATA$state, DATA$person,
#'     demographic=DATA$sex))
#' 
#' lview(as.Corpus(DATA$state, DATA$person,
#'     demographic=list(DATA$sex, DATA$adult)))
#'
#' ## Apply qdap functions meant for dataframes from sentSplit to tm Corpus
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters <- Corpus(DirSource(reut21578),
#'     readerControl = list(reader = readReut21578XML))
#' 
#' matches <- list(
#'     oil = qcv(oil, crude),
#'     money = c("economic", "money")
#' )
#' 
#' apply_as_df(reuters, word_stats)
#' apply_as_df(reuters, formality)
#' apply_as_df(reuters, word_list)
#' apply_as_df(reuters, polarity)
#' apply_as_df(reuters, Dissimilarity)
#' apply_as_df(reuters, diversity)
#' apply_as_df(reuters, pos_by)
#' apply_as_df(reuters, flesch_kincaid)
#' apply_as_df(reuters, trans_venn)
#' apply_as_df(reuters, gantt_plot)
#' apply_as_df(reuters, rank_freq_mplot)
#' apply_as_df(reuters, character_table)
#' 
#' (termco_out <- apply_as_df(reuters, termco, match.list = matches))
#' plot(termco_out, values = TRUE, high="red")
#' 
#' (wordcor_out <- apply_as_df(reuters, word_cor, word = unlist(matches)))
#' plot(wordcor_out)
#' 
#' (f_terms <- apply_as_df(reuters, freq_terms, at.least = 3))
#' plot(f_terms)
#' 
#' apply_as_df(reuters, trans_cloud)
#' ## To use "all" rather than "docs" as "grouping.var"...
#' apply_as_df(reuters, trans_cloud, grouping.var=NULL, 
#'     target.words=matches, cloud.colors = c("red", "blue", "grey75"))
#' 
#' finds <- apply_as_df(reuters, freq_terms, at.least = 5,
#'     top = 5, stopwords = Top100Words)
#' apply_as_df(reuters, dispersion_plot, match.terms = finds[, 1],
#'     total.color = NULL)
#'     
#' ## Filter for Term Document Matrix/Document Term Matrix
#' library(tm)
#' data(crude)
#' 
#' (tdm_in <- TermDocumentMatrix(crude, control = list(stopwords = TRUE)))
#' Filter(tdm_in, 5)
#' 
#' (dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
#' Filter(dtm_in, 5)
#' 
#' ## Filter particular words based on max/min values
#' Filter(dtm_in, 5, 7)
#' Filter(dtm_in, 4, 4)
#' Filter(tdm_in, 3, 4)
#' Filter(tdm_in, 3, 4, stopwords = Top200Words)
#' 
#' ## SPECIAL REMOVAL OF TERMS (more flexible consideration of words than wfm)
#' dat <- data.frame(
#'     person = paste0("person_", 1:5),
#'     tweets = c("test one two", "two apples","hashtag #apple", 
#'         "#apple #tree", "http://microsoft.com")
#' )
#' 
#' ## remove specialty items
#' dat[[2]] <- rm_default(dat[[2]], pattern=pastex("@@rm_url", "#apple\\b"))
#' 
#' 
#' myCorp <- tm::tm_map(crude, tm::removeWords, Top200Words)
#' myCorp %>% as.dtm() %>% tm::inspect()
#' }
as.tdm <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    text.var
    grouping.var
    vowel.check
    
    UseMethod("as.tdm")
}    

#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.TermDocumentMatrix} - Create document term matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#' 
#' @return \code{as.TermDocumentMatrix} - Returns a 
#' \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
as.TermDocumentMatrix <- as.tdm

#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.dtm} - Create document term matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#' 
#' @return \code{as.dtm} - Returns a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname as.tdm
#' @export
as.dtm <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    text.var
    grouping.var
    vowel.check
    
    UseMethod("as.dtm")
} 

#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.DocumentTermMatrix} - Create document term matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#' 
#' @return \code{as.DocumentTermMatrix} - Returns a 
#' \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
as.DocumentTermMatrix <- as.dtm

#' \code{as.tdm.Corpus} - Corpus method for \code{as.tdm} used to 
#' convert to a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.tdm Corpus 
as.tdm.Corpus <- 
function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
    tm::TermDocumentMatrix(x = text.var, ...)
}

#' \code{as.tdm.default} - Default method for \code{as.tdm} used to 
#' convert to a \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.tdm default    
as.tdm.default <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
    tm::as.TermDocumentMatrix(x = text.var, ...)
}

#' \code{as.tdm.character} - character method for \code{as.tdm} used to 
#' convert to a \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.tdm character
as.tdm.character <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    out <- tm_tdm_interface2(text.var = text.var, grouping.var = grouping.var, 
        ...)

    if (vowel.check) {
        out <- out[vowel_check(rownames(out)), ]
    }
    out
}


## Helper function to check on words w/o vowels (matches tm output)
vowel_check <- function(text.var) {
    grepl("[aeiouy]", text.var)
}

#' \code{as.dtm.Corpus} - Corpus method for \code{as.dtm} used to 
#' convert to a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.dtm Corpus 
as.dtm.Corpus <- 
function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
    tm::DocumentTermMatrix(x = text.var, ...)
}

#' \code{as.dtm.default} - Default method for \code{as.dtm} used to 
#' convert to a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.dtm default 
as.dtm.default <- 
function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
    tm::as.DocumentTermMatrix(x = text.var, ...)
}

#' \code{as.dtm.character} - character method for \code{as.dtm} used to 
#' convert to a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.dtm character
as.dtm.character <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    out <- tm_dtm_interface2(text.var = text.var, grouping.var = grouping.var, 
        ...)

    if (vowel.check) {
        out <- out[, vowel_check(colnames(out))]
    }
    out
}

#' \code{as.tdm.wfm} - wfm method for \code{as.tdm} used to 
#' convert to a \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.tdm wfm    
as.tdm.wfm <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    x <- wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...)

    ## Remove rows with terms with no vowel
    if (vowel.check) {
        x <- x[vowel_check(rownames(x)), ,drop=FALSE]
    }

    z <- unlist(apply(x, 2, function(y) sum(y != 0)), use.names = FALSE)

    a <- list(
        unlist(apply(x, 2, function(y) which(y != 0)), use.names = FALSE),
        rep(seq_along(z), z),
        x[apply(x, 2, function(y) y != 0)],
        nrow(x),
        ncol(x),
        dimnames(x)
    )
    
    attributes(a) <- list(
        names = c("i", "j", "v", "nrow", "ncol", "dimnames"),
        class = c("TermDocumentMatrix", "simple_triplet_matrix"),
        weighting = c("term frequency", "tf")
    )
    
    a
}

#' \code{as.dtm.wfm} - wfm method for \code{as.dtm} used to 
#' convert to a \code{\link[tm]{TermDocumentMatrix}}.
#' @rdname as.tdm
#' @export
#' @method as.dtm wfm    
as.dtm.wfm <- 
    function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    x <- t(wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...))

    ## Remove rows with terms with no vowel
    if (vowel.check) {
        x <- x[, vowel_check(colnames(x))]
    }

    z <- unlist(apply(x, 2, function(y) sum(y != 0)), use.names = FALSE)

    a <- list(
        unlist(apply(x, 2, function(y) which(y != 0)), use.names = FALSE),
        rep(seq_along(z), z),
        x[apply(x, 2, function(y) y != 0)],
        nrow(x),
        ncol(x),
        dimnames(x)
    )
    
    attributes(a) <- list(
        names = c("i", "j", "v", "nrow", "ncol", "dimnames")  ,      
        class = c("DocumentTermMatrix", "simple_triplet_matrix"),
        weighting = c("term frequency", "tf")
    )
    
    a
}

wfm2xtab <- function(text.var, grouping.var = NULL, ...) {
  
    if (!methods::is(text.var, "true.matrix")) {
        text.var <- wfm(text.var = text.var, grouping.var = grouping.var, 
            output = "raw", ...)
    }
    
    d <- melt(text.var)
    colnames(d)[1:2] <- c("Terms", "Docs")
    stats::xtabs(value ~ Terms + Docs, d)
}


#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.data.frame} - Convert a \pkg{tm} package \code{\link[tm]{Corpus}} to 
#' a \pkg{qdap} \code{\link[base]{data.frame}}.
#' 
#' @param x A \code{\link[tm]{Corpus}} object.
#' @param doc Name for \code{\link[tm]{Corpus}} documents.
#' @param text Name for \code{\link[tm]{Corpus}} text.
#' @param sent.split logical.  If \code{TRUE} the text variable sentences will 
#' be split into individual rows.
#' @param row.names \code{NULL} or a character vector giving the row names for 
#' the data frame. Not used in \pkg{qdap}; for base generic consistency.
#' @param optional logical. If \code{TRUE}, setting row names and converting 
#' column names is optional. Not used in \pkg{qdap}; for base generic consistency.
#' @return \code{as.data.frame} - Converts a \code{\link[tm]{Corpus}} and returns 
#' a \pkg{qdap} oriented \code{\link[base]{data.frame}}.
#' @rdname as.tdm
#' @export
#' @importFrom qdapTools list2df
#' @method as.data.frame Corpus
as.data.frame.Corpus <- function(x, row.names, optional, ..., doc = "doc_id", 
    text = "text", sent.split = FALSE) {

    if(!methods::is(x[[1]], "PlainTextDocument")) {
        x <- tm::tm_map(x, PlainTextDocument)
    }

    qpaste <- function(x) paste(as.character(x), collapse = " ")

    out <- data.frame(
        qdapTools::list2df(lapply(x, qpaste), col1 = text, col2 = doc)[, 2:1],
        NLP::meta(x),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )


    if (sent.split) {
        if(any(end_mark(out[["text"]]) == "_")) {
            warning("Missing end marks. This may result in lost data.
                \nConsider setting: sent.split = FALSE\n")
        }        
        out <- sentSplit(out, text, ...)
    }
    out
}

#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{as.Corpus} - Attempts to convert its argument into a \pkg{tm} package 
#' \code{\link[tm]{Corpus}}.
#' 
#' @param demographic.vars Additional demographic information about the grouping 
#' variables.  This is a data.frame, list of equal length vectors, or a single 
#' vector corresponding to the grouping variable/text variable.  This 
#' information will be mapped to the DMetaData in the \code{\link[tm]{Corpus}}.
#' @rdname as.tdm
#' @return \code{as.Corpus} - Converts a qdap oriented dataframe and returns 
#' a \code{\link[tm]{Corpus}}.
#' @export
#' @importFrom qdapTools list_df2df
as.Corpus <- function(text.var, grouping.var = NULL, demographic.vars, ...){
    
    text.var
    grouping.var

    UseMethod("as.Corpus")
}    



#' \code{as.Corpus.sent_split} - \code{sent_split} Method for \code{as.Corpus}.
#' @rdname as.tdm
#' @export
#' @method as.Corpus sent_split 
as.Corpus.sent_split <- function(text.var, grouping.var = NULL, 
    demographic.vars, ...){

    if (!is.null(grouping.var) && length(grouping.var) == 1 && 
            is.character(grouping.var)) {
       if (grouping.var %in%  colnames(text.var)) {
           grouping.var <- text.var[, grouping.var]
       }
    }
    if (missing(demographic.vars)){
        nulls <- c(attributes(text.var)[["text.var"]], grouping.var)
        demographic.vars <- text.var[, !colnames(text.var) %in% nulls, 
            drop=FALSE]
    } else {
        if (!is.null(demographic.vars) && is.character(grouping.var)) {
           if (all(demographic.vars %in%  colnames(text.var))) {
               demographic.vars <- text.var[, demographic.vars]
           }
        }   

    }
    as.Corpus.default(
        text.var = text.var[, attributes(text.var)[["text.var"]]],
        grouping.var = grouping.var,
        demographic.vars = demographic.vars
    )
    
}
    
    
#' \code{as.Corpus.default} - Default method for \code{as.Corpus} used to 
#' convert vectors (from a \code{data.frame}) to a \code{\link[tm]{Corpus}}.
#' @rdname as.tdm
#' @export
#' @method as.Corpus default 
as.Corpus.default <- function(text.var, grouping.var = NULL, demographic.vars, 
    ...){

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## convert text.var to character and grouping.var to factor
    DF[, "grouping"] <- factor(DF[, "grouping"])
    DF[, "text.var"] <- as.character(DF[, "text.var"])

    ## Split apart by grouping variables and collapse text
    LST <- sapply(split(DF[, "text.var"], DF[, "grouping"]), 
        paste, collapse = " ")
    # LST_DF <- qdapTools::list2df(LST, "text.var", "grouping")
    LST_DF <- qdapTools::list2df(LST, "text", "doc_id")
   
    # ## Use the tm package to convert to a Corpus
    # mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF), 
    #     readerControl=list(reader=qdap_tm_reader))
    mycorpus <- tm::Corpus(tm::DataframeSource(LST_DF))
    
    ## Add metadata info
    NLP::meta(mycorpus, "MetaID") <- names(LST) #removed 11-12-2017
    NLP::meta(mycorpus, "labels") <- names(LST) #removed 11-12-2017
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        tm::DublinCore(mycorpus, tag = "creator") <- pers
    }

    ## Add other demographic variables to "dmeta"
    if(!missing(demographic.vars)) {
        if (!is.data.frame(demographic.vars)) {
            if (is.list(demographic.vars)) {
                nms <- names(demographic.vars)
                demographic.vars <- do.call(cbind.data.frame, demographic.vars)
                if (is.null(nms)) {
                    colnames(demographic.vars) <- paste0("X", 1:ncol(demographic.vars))
                } else {
                    colnames(demographic.vars) <- nms
                }
            } else {
                if (is.vector(demographic.vars) | is.factor(demographic.vars)) {
                    demographic.vars <- data.frame(dems=demographic.vars)
                } else {
                    warning("Please supply a data.frame, list of equal length vectors,\n",  
                        "   or a single vector to `demographic.vars`")
                }
        
            }
        }
        metadat <- split(demographic.vars, grouping)
        checks <- colSums(do.call(rbind, lapply(metadat, function(x) {
            unlist(lapply(x, function(y) !compare(y)))
        }))) == 0
        if (sum(checks) != 0){
            metadat <- list_df2df(lapply(metadat, 
                function(x) x[1, checks, drop = FALSE]), "MetaID")
            new_vars <- key_merge(NLP::meta(mycorpus)[, 1, drop=FALSE], 
                metadat, defualt.arrange = FALSE)[, -1, drop=FALSE]
            lapply(colnames(new_vars), function(x){
                NLP::meta(mycorpus, x) <<- new_vars[[x]]
            })

        }
    }

    mycorpus
}

## helper readers  ##removed 8-20-2017 b/s tm no longer uses this approach
#qdap_tm_reader <- tm::readTabular(mapping=list(content="text.var", id="grouping"))

compare <- function(v) {
    all(sapply( as.list(v[-1]), FUN=function(z) {identical(z, v[1])}))
}


## ## Transposes a TermDocumentMatrix object
## ## 
## ## Transposes a TermDocumentMatrix object
## ## 
## ## @param x The TermDocumentMatrix object
## ## @param \ldots ignored
## ## @export
## ## @method t TermDocumentMatrix
## t.TermDocumentMatrix <- function(x, ...) {
##      
##     x <- t(as.matrix(x))
## 
##     z <- unlist(apply(x, 2, function(y) sum(y != 0)), use.names = FALSE)
## 
##     a <- list(
##         unlist(apply(x, 2, function(y) which(y != 0)), use.names = FALSE),
##         rep(seq_along(z), z),
##         x[apply(x, 2, function(y) y != 0)],
##         nrow(x),
##         ncol(x),
##         dimnames(x)
##     )
##     
##     attributes(a) <- list(
##         names = c("i", "j", "v", "nrow", "ncol", "dimnames"),
##         class = c("DocumentTermMatrix", "simple_triplet_matrix"),
##         weighting = c("term frequency", "tf")
##     )
##     
##     a
## }

## ## Transposes a DocumentTermMatrix object
## ## 
## ## Transposes a DocumentTermMatrix object
## ## 
## ## @param x The DocumentTermMatrix object
## ## @param \ldots ignored
## ## @export
## ## @method t DocumentTermMatrix
## t.DocumentTermMatrix <- function(x, ...) {
##      
##     x <- t(as.matrix(x))
## 
##     z <- unlist(apply(x, 2, function(y) sum(y != 0)), use.names = FALSE)
## 
##     a <- list(
##         unlist(apply(x, 2, function(y) which(y != 0)), use.names = FALSE),
##         rep(seq_along(z), z),
##         x[apply(x, 2, function(y) y != 0)],
##         nrow(x),
##         ncol(x),
##         dimnames(x)
##     )
##     
##     attributes(a) <- list(
##         names = c("i", "j", "v", "nrow", "ncol", "dimnames"), 
##         class = c("TermDocumentMatrix", "simple_triplet_matrix"),
##         weighting = c("term frequency", "tf")
##     )
##     
##     a
## }




#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{apply_as_tm} - Apply functions intended to be used on the \pkg{tm} 
#' package's \code{\link[tm]{TermDocumentMatrix}} to a \code{\link[qdap]{wfm}} 
#' object.
#' 
#' @param wfm.obj A \code{\link[qdap]{wfm}} object.
#' @param tmfun A function applied to a \code{\link[tm]{TermDocumentMatrix}}
#' object.
#' @param to.qdap logical.  If \code{TRUE} should \code{\link[qdap]{wfm}} try to
#' coerce the output back to a qdap object.
#' @return \code{apply_as_tm} - Applies a tm oriented function to a 
#' \code{\link[qdap]{wfm}} and attempts to simplify back to a 
#' \code{\link[qdap]{wfm}} or \code{weight} format.
#' @rdname as.tdm
#' @export
apply_as_tm <- function(wfm.obj, tmfun, ..., to.qdap = TRUE){

    ## Convert to a tdm
    x <- as.tdm(wfm.obj) 

    ## Apply the tm function
    y <- tmfun(x, ...) 

    ## attempt to coerce back to qdap wfm/weighted_wfm
    if (to.qdap && (methods::is(y, "DocumentTermMatrix")|methods::is(y, "TermDocumentMatrix"))) {
        tm2qdap(y)
    } else {
        y
    }

}

#' Apply a tm Corpus as a qdap Dataframe
#' 
#' \code{apply_as_df} - Apply a \pkg{tm} \code{\link[tm]{Corpus}} as a qdap 
#' dataframe.
#' \code{apply_as_df} - Apply functions intended to be used on the \pkg{qdap} 
#' package's \code{\link[base]{data.frame}} + \code{\link[qdap]{sentSplit}} to 
#' a \pkg{tm} \code{\link[tm]{Corpus}} object.
#' 
#' @param tm.corpus A \code{\link[tm]{Corpus}} object.
#' @param qdapfun A qdap function that is usually used on 
#' text.variable ~ grouping variable.
#' @param stopwords A character vector of words to remove from the text.  qdap 
#' has a number of data sets that can be used as stop words including: 
#' \code{Top200Words}, \code{Top100Words}, \code{Top25Words}.  For the tm 
#' package's traditional English stop words use \code{tm::stopwords("english")}.
#' @param min Minimum word length.
#' @param max Maximum word length.
#' @param count.apostrophe logical.  If \code{TRUE} apostrophes are counted as 
#' characters.
#' @param ignore.case logical.  If \code{TRUE} stop words will be removed 
#' regardless of case.  
#' @note \code{aply_as_df} coerces to a dataframe with columns named `docs` and 
#' the other named `text`.
#' @return \code{apply_as_df} - Returns the output typical of the applied 
#' \pkg{qdap} function.
#' @seealso \code{\link[qdap]{Filter}}
#' @export
#' @rdname as.tdm
apply_as_df <- function(tm.corpus, qdapfun, ..., stopwords = NULL, 
    min = 1, max = Inf, count.apostrophe = TRUE, ignore.case = TRUE) {

    text <- doc <- tot <- NULL

    dat <- as.data.frame(tm.corpus)

    if (!is.null(stopwords)) {
        dat[, "text"] <- rm_stopwords(dat[, "text"], stopwords, separate = FALSE, 
            ignore.case = ignore.case)
    }

    if (min != 1 | max != Inf) {
        dat[, "text"]  <- Filter(dat[, "text"], min = min, max = max, 
            count.apostrophe = count.apostrophe) 
    }
  
    theargs <- names(formals(qdapfun))
   
    ## See if the user has set grouping.var to NULL
    extras <- substitute(...())
    group.null <- FALSE
    if("grouping.var" %in% names(extras)) {
        if (is.null(extras[["grouping.var"]])) {
            group.null <- TRUE
        }
    }

    if (any(theargs %in% "tot")) {
        if (group.null) {
            with(dat, qdapfun(text.var = text, tot = tot, ...)) 
        } else {
            with(dat, qdapfun(text.var = text, grouping.var = doc_id, tot = tot, ...))
        }
    } else {
        if (any(theargs %in% "grouping.var")) {
            if (group.null) {
                with(dat, qdapfun(text.var = text, ...))
            } else {
                with(dat, qdapfun(text.var = text, grouping.var = doc_id, ...))
            }
        } else {
            with(dat, qdapfun(text.var = text, ...))
        }
    }

}


#' Filter
#' 
#' \code{Filter.TermDocumentMatrix} - Filter words from a TermDocumentMatrix vector that meet 
#' max/min word length criteria.
#' 
#' TermDocumentMatrix Method for Filter
#' @rdname Filter
#' @export
#' @method Filter TermDocumentMatrix
#' @return \code{Filter.TermDocumentMatrix} - Returns a matrix of the class "TermDocumentMatrix".
Filter.TermDocumentMatrix <- function(x, min = 1, max = Inf, count.apostrophe = TRUE, 
    stopwords = NULL, ignore.case = TRUE, ...) {
   
    as.tdm(Filter(as.wfm(x), min = min, max = max, 
        count.apostrophe = count.apostrophe, 
        stopwords = stopwords, ignore.case = ignore.case, ...))

}

#' Filter
#' 
#' \code{Filter.DocumentTermMatrix} - Filter words from a DocumentTermMatrix 
#' that meet max/min word length criteria.
#' 
#' DocumentTermMatrix Method for Filter
#' @rdname Filter
#' @export
#' @method Filter DocumentTermMatrix
#' @return \code{Filter.DocumentTermMatrix} - Returns a matrix of the class "DocumentTermMatrix".
Filter.DocumentTermMatrix <- function(x, min = 1, max = Inf, 
    count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...) {
   
    as.dtm(Filter(as.wfm(x), min = min, max = max, 
        count.apostrophe = count.apostrophe, 
        stopwords = stopwords, ignore.case = ignore.case, ...))

}


tm_tdm_interface2 <- function(text.var, grouping.var, stopwords, char2space, 
    apostrophe.remove, ...){

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## convert text.var to character and grouping.var to factor
    DF[, "grouping"] <- factor(DF[, "grouping"])
    DF[, "text.var"] <- as.character(DF[, "text.var"])

    ## Split apart by grouping variables and collapse text
    LST <- sapply(split(DF[, "text.var"], DF[, "grouping"]), 
        paste, collapse = " ")

    # LST_DF <- qdapTools::list2df(LST, "text.var", "id")
    #
    ## Use the tm package to convert to a Corpus
    # mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF), 
    #     readerControl=list(reader=qdap_tm_reader))
    #
    ## Updated approach per tm changes 8/16/2017
  
    LST_DF <- qdapTools::list2df(LST, "text", "doc_id")
    ##mycorpus <- replace_ids(tm::Corpus(tm::DataframeSource(LST_DF)), LST_DF[['id']])
    mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF))  
            
    ## Add metadata info
    NLP::meta(mycorpus, "MetaID") <- names(LST)
    NLP::meta(mycorpus, "labels") <- names(LST)
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        tm::DublinCore(mycorpus, tag = "creator") <- pers
    }

    if(missing(char2space)) char2space <- "~~"

    if(missing(apostrophe.remove)) apostrophe.remove <- FALSE

    apo_rm <- TRUE

    if(!apostrophe.remove) {
        apo_rm <- function(x) gsub(paste0(".*?($|'|", paste(paste0("\\", 
            char2space), collapse = "|"), "|[^[:punct:]]).*?"), 
            "\\1", x)
    }

    if(missing(stopwords)) stopwords <- FALSE

    tm::TermDocumentMatrix(mycorpus,
        control = list(
            removePunctuation = apo_rm,
            wordLengths =c(0, Inf),
            stopwords = FALSE,
            removeNumbers = stopwords
        )
    )

}


# replace_ids <- function(corpus, ids){

#     stopifnot(length(corpus$content) == length(ids))
#     corpus$content <- Map(function(x, y) {
#         x$meta$id <- y
#         x
#     }, corpus$content, ids)
#     corpus
# }

tm_dtm_interface2 <- function(text.var, grouping.var, stopwords, char2space, 
    apostrophe.remove, ...){

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## convert text.var to character and grouping.var to factor
    DF[, "grouping"] <- factor(DF[, "grouping"])
    DF[, "text.var"] <- as.character(DF[, "text.var"])

    ## Split apart by grouping variables and collapse text
    LST <- sapply(split(DF[, "text.var"], DF[, "grouping"]), 
        paste, collapse = " ")

    # LST_DF <- qdapTools::list2df(LST, "text.var", "grouping")
    # 
    # ## Use the tm package to convert to a Corpus
    # mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF), 
    #     readerControl=list(reader=qdap_tm_reader))

    LST_DF <- qdapTools::list2df(LST, "text", "doc_id")
    ## mycorpus <- replace_ids(tm::Corpus(tm::DataframeSource(LST_DF)), LST_DF[['doc_id']])
    mycorpus <- tm::Corpus(tm::DataframeSource(LST_DF))
        
    ## Add metadata info
    NLP::meta(mycorpus, "MetaID") <- names(LST)
    NLP::meta(mycorpus, "labels") <- names(LST)
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        tm::DublinCore(mycorpus, tag = "creator") <- pers
    }

    if(missing(char2space)) char2space <- "~~"

    if(missing(apostrophe.remove)) apostrophe.remove <- FALSE

    apo_rm <- TRUE

    if(!apostrophe.remove) {
        apo_rm <- function(x) gsub(paste0(".*?($|'|", paste(paste0("\\", 
            char2space), collapse = "|"), "|[^[:punct:]]).*?"), 
            "\\1", x)
    }

    if(missing(stopwords)) stopwords <- FALSE

    tm::DocumentTermMatrix(mycorpus,
        control = list(
            removePunctuation = apo_rm,
            wordLengths =c(0, Inf),
            stopwords = stopwords,
            removeNumbers = TRUE
        )
    )
}

#' \code{as.Corpus.TermDocumentMatrix} - \code{TermDocumentMatrix} method for 
#' \code{as.Corpus} used to convert a \code{\link[tm]{Corpus}}.
#' @rdname as.tdm
#' @export
#' @method as.Corpus TermDocumentMatrix 
as.Corpus.TermDocumentMatrix <- function(text.var, ...){

    LST_DF <- qdapTools::list2df(mat2word_list(text.var), "text", "doc_id")

    ## Use the tm package to convert to a Corpus
    # tm::VCorpus(tm::DataframeSource(LST_DF), 
    #     readerControl=list(reader=qdap_tm_reader))
    tm::Corpus(tm::DataframeSource(LST_DF))

}

#' \code{as.Corpus.DocumentTermMatrix} - \code{DocumentTermMatrix} method for 
#' \code{as.Corpus} used to convert a \code{\link[tm]{Corpus}}.
#' @rdname as.tdm
#' @export
#' @method as.Corpus DocumentTermMatrix 
as.Corpus.DocumentTermMatrix <- function(text.var, ...){

    as.Corpus.TermDocumentMatrix(t(text.var))
 
}

#' \code{as.Corpus.wfm} - \code{wfm} method for 
#' \code{as.Corpus} used to convert a \code{\link[tm]{Corpus}}.
#' @rdname as.tdm
#' @export
#' @method as.Corpus wfm 
as.Corpus.wfm <- function(text.var, ...){

    as.Corpus.TermDocumentMatrix(as.tdm(text.var))

}

## helper function to construct Corpus from matrices
mat2word_list <- function(mat){

    apply(mat, 2, function(x) {
        unbag(rep(names(x), x))
    })

}

##Removed after las archived:
## ## Latent Semantic Analysis
## library(lsa)
## lsa(as.tdm(x), dims=dimcalc_share())
## lsa(as.tdm(DATA$state, DATA$person), dims=dimcalc_share())

#' 
#' out <- lsa(DocTermMat2, 6)
#' out$tk
#' out2 <- colsplit2df(matrix2df(out$tk), new.names = qcv(Person, Time))
#' out2$Person <- factor(out2$Person, 
#'     levels = names(sort(colSums(with(pres_debates2012, 
#'         wfm(dialogue, person, stopwords = SW))), TRUE))
#' )
#' colnames(out2) <- gsub("X", "Topic ", colnames(out2))
#' 
#' qheat(out2, facet.vars = "Time", high="darkgreen", plot=FALSE) + 
#'     theme(legend.title=element_blank()) + 
#'     guides(fill = guide_colorbar(barwidth = .5, barheight = 12))
