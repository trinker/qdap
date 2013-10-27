#' tm Package Compatability Tools: Aplly to or Convert to/from Term Document Matrix or Document Term Matrix
#' 
#' \code{tdm} - Create term document matrices from raw text or \code{wfm} for 
#' use with other text analysis packages.
#'
#' @param text.var The text variable or a \code{wfm} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots If \code{tdm} or \code{dtm} - Other arguments passed to 
#' \code{wfm}.  If \code{apply_as_tm} - Other arguments passed to functions used 
#' on the tm package's \code{"TermDocumentMatrix"}.
#' @details Identical to the \code{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
#' @return \code{tdm} - Returns a \code{\link[tm]{TermDocumentMatrix}}.
#' @export
#' @importFrom reshape2 melt
#' @importFrom tm tm_map as.PlainTextDocument
#' @rdname tdm
#' @examples
#' \dontrun{
#' dtm(DATA$state, DATA$person)
#' tdm(DATA$state, DATA$person)
#' 
#' x <- wfm(DATA$state, DATA$person)
#' tdm(x)
#' dtm(x)
#' library(tdm)
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
#' ## tm Matrices to wfm
#' library(tm)
#'
#' ## A Term Document Matrix Conversion
#' (tm_in <- TermDocumentMatrix(crude, control = list(stopwords = TRUE)))
#' converted <- tm2wfm(tm_in)
#' head(converted)
#' summary(converted)
#'
#' ## A Document Term Matrix Conversion
#' (dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
#' summary(tm2wfm(dtm_in))
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
#' apply_as_tm(a, tm:::Dictionary)
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
#' ## Convert to data.frame
#' corp_df <- tm_corpus2df(reuters)
#' htruncdf(corp_df)
#' 
#' ## Apply a qdap function
#' out <- formality(corp_df$text, corp_df$doc3)
#' plot(out)
#' }
tdm <- function(text.var, grouping.var = NULL, ...) {

    x <- wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...)

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



#' tm Package Compatability Tools: Aplly to or Convert to/from Term Document Matrix or Document Term Matrix
#' 
#' \code{dtm} - Create document term matrices from raw text or \code{wfm} for 
#' use with other text analysis packages.
#' 
#' @return \code{dtm} - Returns a \code{\link[tm]{DocumentTermMatrix}}.
#' @rdname tdm
#' @export
dtm <- function(text.var, grouping.var = NULL, ...) {

    x <- t(wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...))

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


#' tm Package Compatability Tools: Aplly to or Convert to/from Term Document Matrix or Document Term Matrix
#' 
#' \code{tm2wfm} - Convert the \code{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}} to
#' \code{\link[qdap]{wfm}}.
#' 
#' @param x A \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
#' @return \code{tm2qdap} - Returns a \code{\link[qdap]{wfm}} object or 
#' \code{\link[qdap]{wfm_weight}} object.
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


#' tm Package Compatability Tools: Apply to or Convert to/from Term Document Matrix or Document Term Matrix
#' 
#' \code{apply_as_tm} - Apply functions intended to be used on the \code{tm} 
#' package's \code{\link[tm]{TermDocumentMatrix}} to a \code{\link[qdap]{wfm}} 
#' object.
#' 
#' @param wfm.obj A \code{\link[qdap]{wfm}} object.
#' @param tmfun A function applied to a \code{\link[tm]{TermDocumentMatrix}}
#' object.
#' @param to.qdap logical.  If \code{TRUE} should \code{\link[qdap]{wfm}} try to
#' coerce the output back to a qdap object.
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

#' tm Package Compatability Tools: Apply to or Convert to/from Term Document Matrix or Document Term Matrix
#' 
#' \code{tm_corpus2df} - Convert a tm package corpus to a dataframe.
#' 
#' @param tm.corpus A \code{\link[tm]{Corpus}} object.
#' @param col1 Name for column 1 (the vector elements).
#' @param col2 Name for column 2 (the names of the vectors).
#' @rdname tdm
#' @export
tm_corpus2df <- function(tm.corpus, col1 = "docs", col2 = "text") {

    if(!is(tm.corpus[[1]], "PlainTextDocument")) {
        tm.corpus <- tm_map(tm.corpus, as.PlainTextDocument)
    }
    list2df(tm.corpus, "text", "doc")[, 2:1]
}
