#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{tdm} - Create term document matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#'
#' @param text.var The text variable or a \code{\link[qdap]{wfm}} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots If \code{tdm} or \code{dtm} - Other arguments passed to 
#' \code{wfm}.  If \code{apply_as_tm} - Other arguments passed to functions used 
#' on the tm package's \code{"TermDocumentMatrix"}.  If \code{df2tm_corpus} - 
#' Other arguments passed to the tm package's \code{\link[tm]{Corpus}}.  If 
#' \code{tm_corpus2wfm} - Other arguments passed to \code{\link[qdap]{wfm}}.
#' @param vowel.check logical.  Should terms without vowels be remove?  
#' @details Produces output that is identical to the \code{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}, \code{\link[tm]{DocumentTermMatrix}},
#' \code{\link[tm]{Corpus}} or allows convenient interface between the qdap and 
#' tm packages.
#' @return \code{tdm} - Returns a \code{\link[tm]{TermDocumentMatrix}}.
#' @export
#' @seealso \code{\link[tm]{DocumentTermMatrix}},
#' \code{\link[tm]{Corpus}},
#' \code{\link[tm]{TermDocumentMatrix}}
#' @importFrom reshape2 melt
#' @importFrom tm tm_map as.PlainTextDocument VectorSource Corpus
#' @rdname tdm
#' @examples
#' \dontrun{
#' dtm(DATA$state, DATA$person)
#' tdm(DATA$state, DATA$person)
#' 
#' x <- wfm(DATA$state, DATA$person)
#' tdm(x)
#' dtm(x)
#' library(tm)
#' plot(tdm(x))
#' 
#' pres <- tdm(pres_debates2012$dialogue, pres_debates2012$person)
#' plot(pres, corThreshold = 0.8)
#' pres
#' (pres2 <- removeSparseTerms(pres, .3))
#' plot(pres2, corThreshold = 0.95)
#' 
#' ## Latent Semantic Analysis
#' library(lsa)
#' lsa(tdm(x), dims=dimcalc_share())
#' lsa(tdm(DATA$state, DATA$person), dims=dimcalc_share())
#' 
#' shorts <- all_words(pres_debates2012)[,1][nchar(all_words(
#'     pres_debates2012)[,1]) < 4]
#' 
#' SW <- c(shorts, qdapDictionaries::contractions[, 1],
#'     qdapDictionaries::Top200Words,
#'     "governor", "president", "mister", "obama","romney")
#' 
#' DocTermMat2 <- with(pres_debates2012, dtm(dialogue, list(person, time), stopwords = SW))
#' DocTermMat2 <- removeSparseTerms(DocTermMat2,0.95)
#' DocTermMat2 <- DocTermMat2[rowSums(as.matrix(DocTermMat2))> 0,]
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
#'     
#' ## Correspondence Analysis
#' library(ca)
#' 
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' speech <- stemmer(dat$dialogue)
#' mytable1 <- with(dat, tdm(speech, list(person, time), stopwords = Top25Words))
#' 
#' fit <- ca(mytable1)
#' summary(fit)
#' plot(fit)
#' plot3d.ca(fit, labels=1)
#' 
#' 
#' mytable2 <- with(dat, tdm(speech, list(person, time), stopwords = Top200Words))
#' 
#' fit2 <- ca(mytable2)
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
#' DocTermMat <- with(pres_debates2012, dtm(dialogue, person, stopwords = SW))
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
#' DocTermMat2 <- with(pres_debates2012, dtm(dialogue, list(person, time), stopwords = SW))
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
#' grid.arrange(p1, p2, nrow=1, widths = c(.85, .15))
#'     
#' ## tm Matrices to wfm
#' library(tm)
#' data(crude)
#'
#' ## A Term Document Matrix Conversion
#' (tm_in <- TermDocumentMatrix(crude, control = list(stopwords = TRUE)))
#' converted <- tm2qdap(tm_in)
#' head(converted)
#' summary(converted)
#'
#' ## A Document Term Matrix Conversion
#' (dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
#' summary(tm2qdap(dtm_in))
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
#' apply_as_tm(a, tm:::dissimilarity, method = "cosine")
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
#' corp_df <- tm_corpus2df(reuters)
#' htruncdf(corp_df)
#' 
#' ## Apply a qdap function
#' out <- formality(corp_df$text, corp_df$docs)
#' plot(out)
#' 
#' ## Convert a qdap dataframe to tm package Corpus
#' (x <- with(DATA2, df2tm_corpus(state, list(person, class, day))))
#' library(tm)
#' inspect(x)
#' class(x)
#' 
#' (y <- with(pres_debates2012, df2tm_corpus(dialogue, list(person, time))))
#' 
#' ## Add demographic info to DMetaData of Corpus
#' z <- df2tm_corpus(DATA$state, DATA$person, 
#'     demographic=DATA[, qcv(sex, adult, code)])
#' lview(z)
#' 
#' lview(df2tm_corpus(DATA$state, DATA$person,
#'     demographic=DATA$sex))
#' 
#' lview(df2tm_corpus(DATA$state, DATA$person,
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
#' }
tdm <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {

    x <- wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...)

    ## Remove rows with terms with no vowel
    if (vowel.check) {
        x <- x[vowel_check(rownames(x)), ]
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
            class = c("TermDocumentMatrix", "simple_triplet_matrix"),
            Weighting = c("term frequency", "tf")
    )
    
    names(a) <- c("i", "j", "v", "nrow", "ncol", "dimnames")
    a
}

## Helper function to check on words w/o vowels (matches tm output)
vowel_check <- function(text.var) {
    grepl("[aeiouy]", text.var)
}



#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{dtm} - Create document term matrices from raw text or 
#' \code{\link[qdap]{wfm}} for use with other text analysis packages.
#' 
#' @return \code{dtm} - Returns a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname tdm
#' @export
dtm <- 
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
            class = c("DocumentTermMatrix", "simple_triplet_matrix"),
            Weighting = c("term frequency", "tf")
    )
    
    names(a) <- c("i", "j", "v", "nrow", "ncol", "dimnames")
    a
}


wfm2xtab <- function(text.var, grouping.var = NULL, ...) {
  
    if (!is(text.var, "true.matrix")) {
        text.var <- wfm(text.var = text.var, grouping.var = grouping.var, 
            output = "raw", ...)
    }
    
    d <- melt(text.var)
    colnames(d)[1:2] <- c("Terms", "Docs")
    xtabs(value ~ Terms + Docs, d)
}


#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{tm2qdap} - Convert the \pkg{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}} to
#' \code{\link[qdap]{wfm}}.
#' 
#' @param x A \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
#' @return \code{tm2qdap} - Returns a \code{\link[qdap]{wfm}} object or 
#' \code{weight} object.
#' @rdname tdm
#' @export
tm2qdap <- function(x) {

    opts <- c("DocumentTermMatrix", "TermDocumentMatrix")
    cls <- opts[opts %in% class(x)]

    if (cls == "DocumentTermMatrix") {
        x <- t(x)
    }
    
    y <- as.matrix(data.frame(as.matrix(x), check.names = FALSE))
    
    if(!any(attributes(x)[["Weighting"]] %in% "tf")){
        class(y) <- c("weighted_wfm", class(y))
    } else {
        class(y) <- c("wfm", "true.matrix", class(y))
    }

    y

}



#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{tm_corpus2df} - Convert a tm package corpus to a dataframe.
#' 
#' @param tm.corpus A \code{\link[tm]{Corpus}} object.
#' @param col1 Name for column 1 (the vector elements).
#' @param col2 Name for column 2 (the names of the vectors).
#' @return \code{tm_corpus2df} - Converts a \code{\link[tm]{Corpus}} and returns 
#' a qdap oriented dataframe.
#' @rdname tdm
#' @export
tm_corpus2df <- function(tm.corpus, col1 = "docs", col2 = "text") {

    if(!is(tm.corpus[[1]], "PlainTextDocument")) {
        tm.corpus <- tm_map(tm.corpus, as.PlainTextDocument)
    }
    
    list2df(tm.corpus, col1 = col2, col2 = col1)[, 2:1]
}

#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{tm_corpus2wfm} - Convert a \code{\link[tm]{Corpus}} package corpus to a 
#' \code{\link[qdap]{wfm}}. 
#' 
#' @rdname tdm
#' @return \code{df2tm_wfm} - Converts a qdap oriented dataframe and returns 
#' a \code{\link[qdap]{wfm}}.
#' @export
tm_corpus2wfm <- function(tm.corpus, col1 = "docs", col2 = "text", ...) {

      text <- docs <- NULL
      with(tm_corpus2df(tm.corpus), wfm(text, docs, ...))  

}


#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{df2tm_corpus} - Convert a qdap dataframe to a tm package 
#' \code{\link[tm]{Corpus}}.
#' 
#' @param demographic.vars Additional demographic information about the grouping 
#' variables.  This is a data.frame, list of equal length vectors, or a single 
#' vector corresponding to the grouping variable/text variable.  This 
#' information will be mapped to the DMetaData in the \code{\link[tm]{Corpus}}.
#' @rdname tdm
#' @return \code{df2tm_corpus} - Converts a qdap oriented dataframe and returns 
#' a \code{\link[tm]{Corpus}}.
#' @export
df2tm_corpus <- function(text.var, grouping.var = NULL, demographic.vars, ...){

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

    ## Use the tm package to convert to a Corpus
    mycorpus <- Corpus(VectorSource(LST), ...)
    
    ## Add metadata info
    attributes(mycorpus)[["DMetaData"]][,1] <- names(LST)
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        attributes(mycorpus)[["CMetaData"]][["MetaData"]][["creator"]] <- pers
    }

    ## Add other demographic variables to "DMetaData"
    if(!missing(demographic.vars)) {
        if (is.data.frame(demographic.vars)) {
    
        } else {
            if (is.list(demographic.vars)) {
                nms <- colnames(demographic.vars)
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
            attributes(mycorpus)[["DMetaData"]] <- 
                key_merge(attributes(mycorpus)[["DMetaData"]], metadat)
        }
    }

    mycorpus
}


compare <- function(v) all(sapply( as.list(v[-1]), 
    FUN=function(z) {identical(z, v[1])}))


#' Transposes a TermDocumentMatrix object
#' 
#' Transposes a TermDocumentMatrix object
#' 
#' @param x The TermDocumentMatrix object
#' @param \ldots ignored
#' @S3method t TermDocumentMatrix
#' @method t TermDocumentMatrix
t.TermDocumentMatrix <- function(x, ...) {
     
    x <- t(as.matrix(x))

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
            class = c("DocumentTermMatrix", "simple_triplet_matrix"),
            Weighting = c("term frequency", "tf")
    )
    
    names(a) <- c("i", "j", "v", "nrow", "ncol", "dimnames")
    a
}

#' Transposes a DocumentTermMatrix object
#' 
#' Transposes a DocumentTermMatrix object
#' 
#' @param x The DocumentTermMatrix object
#' @param \ldots ignored
#' @S3method t DocumentTermMatrix
#' @method t DocumentTermMatrix
t.DocumentTermMatrix <- function(x, ...) {
     
    x <- t(as.matrix(x))

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
            class = c("TermDocumentMatrix", "simple_triplet_matrix"),
            Weighting = c("term frequency", "tf")
    )
    
    names(a) <- c("i", "j", "v", "nrow", "ncol", "dimnames")
    a
}

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
#' @rdname tdm
#' @export
apply_as_tm <- function(wfm.obj, tmfun, ..., to.qdap = TRUE){

    ## Convert to a tdm
    x <- tdm(wfm.obj) 

    ## Apply the tm function
    y <- tmfun(x, ...) 

    ## attempt to coerce back to qdap wfm/weighted_wfm
    if (to.qdap && (is(y, "DocumentTermMatrix")|is(y, "TermDocumentMatrix"))) {
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
#' @note \code{aply_ad_df} coerces to a dataframe with columns named `docs` and 
#' the other named `text`.
#' @seealso \code{\link[qdap]{Filter}}
#' @export
#' @rdname tdm
apply_as_df <- function(tm.corpus, qdapfun, ..., stopwords = NULL, 
    min = 1, max = Inf, count.apostrophe = TRUE, ignore.case = TRUE) {

    text <- doc <- tot <- NULL

    dat <- sentSplit(tm_corpus2df(tm.corpus), "text")

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
            with(dat, qdapfun(text.var = text, grouping.var = docs, tot = tot, ...))
        }
    } else {
        if (any(theargs %in% "grouping.var")) {
            if (group.null) {
                with(dat, qdapfun(text.var = text, ...))
            } else {
                with(dat, qdapfun(text.var = text, grouping.var = docs, ...))
            }
        } else {
            with(dat, qdapfun(text.var = text, ...))
        }
    }

}

