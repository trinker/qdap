## #' Deprecated qdap Functions
## #' 
## #' \code{df2tm_corpus} - Convert a qdap dataframe to a tm package 
## #' \code{\link[tm]{Corpus}}.
## #' 
## #' @param text.var The text variable or a \code{\link[qdap]{wfm}} object.
## #' @param grouping.var The grouping variables.  Default \code{NULL} generates 
## #' one word list for all text.  Also takes a single grouping variable or a list 
## #' of 1 or more grouping variables.
## #' @param demographic.vars Additional demographic information about the grouping 
## #' variables.  This is a data.frame, list of equal length vectors, or a single 
## #' vector corresponding to the grouping variable/text variable.  This 
## #' information will be mapped to the DMetaData in the \code{\link[tm]{Corpus}}.
## #' @param \ldots Other arguments passed to \code{\link[qdap]{sentSplit}}.  
## #' @rdname deprecated
## #' @section Warning: \code{df2tm_corpus} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.Corpus} instead.
## #' @export
## #' @importFrom qdapTools list_df2df
## df2tm_corpus <- function(text.var, grouping.var = NULL, demographic.vars, ...){
## 
##     .Deprecated(msg = paste("`df2tm_corpus` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.Corpus` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])    
##     
##     if(is.null(grouping.var)) {
##         G <- "all"
##     } else {
##         if (is.list(grouping.var)) {
##             m <- unlist(as.character(substitute(grouping.var))[-1])
##             m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
##                     x[length(x)]
##                 }
##             )
##             G <- paste(m, collapse="&")
##         } else {
##             G <- as.character(substitute(grouping.var))
##             G <- G[length(G)]
##         }
##     }
##     if(is.null(grouping.var)){
##         grouping <- rep("all", length(text.var))
##     } else {
##         if (is.list(grouping.var) & length(grouping.var)>1) {
##             grouping <- paste2(grouping.var)
##         } else {
##             grouping <- unlist(grouping.var)
##         } 
##     } 
##     DF <- data.frame(grouping, text.var, check.names = FALSE, 
##         stringsAsFactors = FALSE)
## 
##     ## convert text.var to character and grouping.var to factor
##     DF[, "grouping"] <- factor(DF[, "grouping"])
##     DF[, "text.var"] <- as.character(DF[, "text.var"])
## 
##     ## Split apart by grouping variables and collapse text
##     LST <- sapply(split(DF[, "text.var"], DF[, "grouping"]), 
##         paste, collapse = " ")
## 
##     ## Use the tm package to convert to a Corpus
##     mycorpus <- Corpus(VectorSource(LST), ...)
##     
##     ## Add metadata info
##     attributes(mycorpus)[["DMetaData"]][,1] <- names(LST)
##     pers <- unname(Sys.info()["user"])
##     if (!is.null(pers)) {
##         attributes(mycorpus)[["CMetaData"]][["MetaData"]][["creator"]] <- pers
##     }
## 
##     ## Add other demographic variables to "DMetaData"
##     if(!missing(demographic.vars)) {
##         if (is.data.frame(demographic.vars)) {
##     
##         } else {
##             if (is.list(demographic.vars)) {
##                 nms <- colnames(demographic.vars)
##                 demographic.vars <- do.call(cbind.data.frame, demographic.vars)
##                 if (is.null(nms)) {
## 
##                     colnames(demographic.vars) <- paste0("X", 1:ncol(demographic.vars))
##                 } else {
##                     colnames(demographic.vars) <- nms
##                 }
##             } else {
##                 if (is.vector(demographic.vars) | is.factor(demographic.vars)) {
##                     demographic.vars <- data.frame(dems=demographic.vars)
##                 } else {
##                     warning("Please supply a data.frame, list of equal length vectors,\n",  
##                         "   or a single vector to `demographic.vars`")
##                 }
##         
##             }
##         }
##         metadat <- split(demographic.vars, grouping)
##         checks <- colSums(do.call(rbind, lapply(metadat, function(x) {
##             unlist(lapply(x, function(y) !compare(y)))
##         }))) == 0
##         if (sum(checks) != 0){
##             metadat <- list_df2df(lapply(metadat, 
##                 function(x) x[1, checks, drop = FALSE]), "MetaID")
##             attributes(mycorpus)[["DMetaData"]] <- 
##                 key_merge(attributes(mycorpus)[["DMetaData"]], metadat)
##         }
##     }
## 
##     mycorpus
## }
## 
## 
## 
## #' Deprecated qdap Functions
## #' 
## #' \code{tm2qdap} - Convert the \pkg{tm} package's 
## #' \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}} to
## #' \code{\link[qdap]{wfm}}.
## #' 
## #' @param x A \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
## #' @section Warning: \code{tm2qdap} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.wfm} instead. 
## #' @rdname deprecated
## #' @export
## tm2qdap <- function(x) {
## 
##     .Deprecated(msg = paste("`tm2qdap` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.wfm` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])   
##     
##     opts <- c("DocumentTermMatrix", "TermDocumentMatrix")
##     cls <- opts[opts %in% class(x)]
## 
##     if (cls == "DocumentTermMatrix") {
##         x <- t(x)
##     }
##     
##     y <- as.matrix(data.frame(as.matrix(x), check.names = FALSE))
##     
##     if(!any(attributes(x)[["weighting"]] %in% "tf")){
##         class(y) <- c("weighted_wfm", class(y))
##     } else {
##         class(y) <- c("wfm", "true.matrix", class(y))
##     }
## 
##     y
## 
## }
## 
## #' Deprecated qdap Functions
## #' 
## #' \code{tm_corpus2wfm} - Convert a \code{\link[tm]{Corpus}} package corpus to a 
## #' \code{\link[qdap]{wfm}}. 
## #' 
## #' @rdname deprecated
## #' @section Warning: \code{tm_corpus2wfm} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.wfm} instead.  
## #' @export
## tm_corpus2wfm <- function(tm.corpus, col1 = "docs", col2 = "text", ...) {
## 
##     .Deprecated(msg = paste("`tm2qdap` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.wfm` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])   
##     
##       text <- docs <- NULL
##       with(as.data.frame(tm.corpus, col1 = col1, col2 = col2), wfm(text, docs, ...))  
## 
## }
## 
## 
## #' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
## #' Matrix or Document Term Matrix
## #' 
## #' \code{tm_corpus2df} - Convert a tm package corpus to a dataframe.
## #' 
## #' @param tm.corpus A \code{\link[tm]{Corpus}} object.
## #' @param col1 Name for column 1 (the vector elements).
## #' @param col2 Name for column 2 (the names of the vectors).
## #' @param sent.split logical.  If \code{TRUE} the text variable sentences will 
## #' be split into individual rows.
## #' @section Warning: \code{tm_corpus2df} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.data.frame} instead.  
## #' @rdname deprecated
## #' @export
## #' @importFrom qdapTools list2df
## tm_corpus2df <- function(tm.corpus, col1 = "docs", col2 = "text", 
##     sent.split = TRUE, ...) {
## 
##     .Deprecated(msg = paste("`tm_corpus2df` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.data.frame` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])   
##     
##     
##     if(!is(tm.corpus[[1]], "PlainTextDocument")) {
##         tm.corpus <- tm_map(tm.corpus, PlainTextDocument)
##     }
##     
##     out <- list2df(tm.corpus, col1 = col2, col2 = col1)[, 2:1]
## 
##     metadat <- attributes(tm.corpus)[["DMetaData"]]
##     if (ncol(metadat) > 1) {
##         colnames(metadat)[1] <- col1
##         out <- key_merge(out, metadat)
##     }
## 
##     if (sent.split) {
##         out <- sentSplit(out, col2, ...)
##     }
##     out
## 
## }
## 
## 
## 
## #' Deprecated qdap Functions
## #' 
## #' \code{tdm} - Create term document matrices from raw text or 
## #' \code{\link[qdap]{wfm}} for use with other text analysis packages.
## #'
## #' @param vowel.check logical.  Should terms without vowels be remove?  
## #' @export
## #' @section Warning: \code{tdm} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.tdm} instead.  
## #' @importFrom reshape2 melt
## #' @importFrom tm tm_map PlainTextDocument VectorSource Corpus
## #' @rdname deprecated
## tdm <- function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
## 
##     .Deprecated(msg = paste("`tdm` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.tdm` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L]) 
##     
##     x <- wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...)
## 
##     ## Remove rows with terms with no vowel
##     if (vowel.check) {
##         x <- x[vowel_check(rownames(x)), ]
##     }
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
## 
## #' Deprecated qdap Functions
## #' 
## #' \code{dtm} - Create document term matrices from raw text or 
## #' \code{\link[qdap]{wfm}} for use with other text analysis packages.
## #' 
## #' @section Warning: \code{dtm} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{as.dtm} instead.
## #' @rdname deprecated
## #' @export
## dtm <- 
## function(text.var, grouping.var = NULL, vowel.check = TRUE, ...) {
## 
##     .Deprecated(msg = paste("`dtm` is deprecated and will be removed in", 
##         "a subsequent version of qdap.  Please use `as.dtm` instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])   
##     
##     x <- t(wfm2xtab(text.var = text.var, grouping.var = grouping.var, ...))
## 
##     ## Remove rows with terms with no vowel
##     if (vowel.check) {
##         x <- x[, vowel_check(colnames(x))]
##     }
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
## 
## #' Polarity Score (Sentiment Analysis)
## #' 
## #' \code{polarity_frame} - Generate a polarity lookup hash key 
## #' for use with the \code{polarity.frame} argument in the \code{polarity} 
## #' function.
## #' 
## #' @param positives A character vector of positive words.
## #' @param negatives A character vector of negative words.
## #' @param pos.weights A vector of weights to weight each positive word by.  
## #' Length must be equal to length of \code{postives} or length 1 (if 1 weight 
## #' will be recycled). 
## #' @param neg.weights A vector of weights to weight each negative word by.  
## #' Length must be equal to length of \code{negatives} or length 1 (if 1 weight 
## #' will be recycled). 
## #' @export
## #' @section Warning: \code{polarity_frame} - function is deprecated.  It will be 
## #' removed in a subsequent version of qdap.  Use \code{sentiment_frame} instead.
## #' @rdname deprecated
## polarity_frame <- function(positives, negatives, pos.weights = 1, 
##     neg.weights = -1) {
##     
##     .Deprecated(msg = paste("polarity_frame is deprecated.  Please use the", 
##         "sentiment_frame function instead."), 
##         old = as.character(sys.call(sys.parent()))[1L])    
##     
##     plen <- length(positives)
##     nlen <- length(negatives)
##     if (!length(plen) %in% c(length(positives), 1)) {
##         stop("The length of positives and pos.weights must be equal")
##     }
##     if (!length(nlen) %in% c(length(negatives), 1)) {
##         stop("The length of negatives and negative weights must be equal")
##     }
##     if (length(pos.weights) == 1) {
##         pos.weights <- rep(pos.weights, plen)
##     }
##     if (length(neg.weights) == 1) {
##         neg.weights <- rep(neg.weights, nlen)
##     }
##     dat <- data.frame(words = c(positives, negatives), polarity = c(pos.weights, 
##         neg.weights), stringsAsFactors = FALSE)
##     hash(dat)
## }
