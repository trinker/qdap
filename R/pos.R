#' Parts of Speech Tagging
#' 
#' \code{pos} - Apply part of speech tagger to transcript(s).
#' 
#' @param text.var The text variable.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.
#' @param na.omit logical.  If \code{TRUE} missing values (\code{NA}) will be 
#' omitted.
#' @param progress.bar logical.  If \code{TRUE} attempts to provide a OS 
#' appropriate progress bar.  If parallel is \code{TRUE} this argument is 
#' ignored. Note that setting this argument to \code{TRUE} may slow down the 
#' function.
#' @param digits Integer; number of decimal places to round when printing.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param zero.replace Value to replace 0 values with.
#' @param gc.rate An integer value.  This is a necessary argument because of a 
#' problem with the garbage collection in the openNLP function that 
#' \code{\link[qdap]{pos}} wraps.  Consider adjusting this argument upward if 
#' the error \code{java.lang.OutOfMemoryError} occurs.
#' @return \code{pos} -  returns a list of 4: 
#' \item{text}{The original text} 
#' \item{POStagged}{The original words replaced with parts of speech in context.} 
#' \item{POSprop}{Dataframe of the proportion of parts of speech by row.} 
#' \item{POSfreq}{Dataframe of the frequency of parts of speech by row.} 
#' \item{POSrnp}{Dataframe of the frequency and proportions of parts of speech 
#' by row.} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @rdname pos
#' @note Note that contractions are treated as two words; for example the word 
#' count on \bold{"what's"} is 2 for \bold{"what + is"}.  This is not consistent 
#' with the \code{\link[qdap]{word_count}} treatment of contractions but makes 
#' sense in a part of speech framework where a phrase such as "She's cool" is 
#' treated as a pronoun, verb and adjective respectively for "She + is + cool".
#' @seealso \code{\link[openNLP]{Maxent_POS_Tag_Annotator}},
#' \code{\link[qdap]{colcomb2class}}
#' @references http:/opennlp.apache.org
#' @keywords parts-of-speech
#' @export
#' @importFrom parallel parLapply makeCluster detectCores stopCluster clusterEvalQ clusterExport
#' @importFrom openNLP Maxent_POS_Tag_Annotator Maxent_Word_Token_Annotator
#' @importFrom NLP as.String annotate Annotation
#' @importFrom qdapTools mtabulate
#' @examples 
#' \dontrun{
#' posdat <- pos(DATA$state)
#' ltruncdf(posdat, 7, 4)
#' ## str(posdat)
#' names(posdat)
#' posdat$text           #original text
#' 
#' ## Methods
#' preprocessed(posdat)  #words replaced with parts of speech
#' counts(posdat)        #frequency of parts of speech by row
#' proportions(posdat)   #proportion of parts of speech by row
#' 
#' ## Methods Plotting
#' plot(preprocessed(posdat))
#' plot(counts(posdat))
#' plot(proportions(posdat))
#' plot(posdat)
#' 
#' out1 <- pos(DATA$state, parallel = TRUE) # not always useful
#' ltruncdf(out1, 7, 4)
#' 
#' #use pos_tags to interpret part of speech tags used by pos & pos_by
#' pos_tags()[1:10, ]
#' pos_tags("matrix")[1:10, ]
#' pos_tags("dataframe")[1:10, ]
#' pos_tags("df")[1:10, ]
#' ltruncdf(pos_tags("all"), 3)
#' 
#' posbydat <- with(DATA, pos_by(state, sex))
#' names(posbydat)
#' 
#' ## Methods
#' scores(posbydat)   
#' preprocessed(posbydat)
#' counts(posbydat)     
#' proportions(posbydat)   
#' 
#' ## Methods Plotting
#' plot(preprocessed(posbydat))
#' plot(counts(posbydat))
#' plot(proportions(posbydat))
#' plot(posbydat)
#' 
#' ltruncdf(posbydat, 7, 4)
#' truncdf(posbydat$pos.by.prop, 4)
#' 
#' POSby <- with(DATA, pos_by(state, list(adult, sex)))
#' plot(POSby, values = TRUE, digits = 2)
#' #or more quickly - reuse the output from before
#' out2 <- with(DATA, pos_by(posbydat, list(adult, sex)))
#'
#' ## Definite/Indefinite Noun 
#' ## 2 approached compared...
#' ## The later is more efficient but less accurate
#' 
#' ## ------------------------##
#' ## Part off speech tagging ##
#' ## ------------------------##
#' pos_after <- function(text.var, words, pos){
#' 
#'     posses <- strsplit(as.character(text.var[["POStagged"]][["POStagged"]]), "\\s+")
#'     namespos <- lapply(posses, function(x) {
#'         y <- unlist(strsplit(x, "/"))
#'         setNames(y[c(TRUE, FALSE)], y[c(FALSE, TRUE)])
#'     })
#' 
#'     lapply(namespos, function(x, thewords = words, thepos = pos){
#'         locs <- which(x %in% thewords)
#'         locs <- locs[!is.na(locs)]
#' 
#'         if (identical(unclass(locs), integer(0))) return(NA_character_)
#' 
#'         nounlocs <- which(names(x) %in% thepos)
#' 
#'         unname(x[unique(sapply(locs, function(x){ 
#'             min(nounlocs[nounlocs - x > 0])
#'         }))])
#'     })  
#' }
#' 
#' out2 <- setNames(lapply(list(a=c("a", "an"), the="the"), function(x) {
#'     o <- pos_after(rajPOS, x, c("NN", "NNS", "NNP", "NNPS"))
#'     m <- stats::setNames(data.frame(sort(table(unlist(o))), 
#'         stringsAsFactors = FALSE), c("word", "freq"))
#'     m[m$freq> 3, ]
#' }), c("a", "the"))
#' 
#' 
#' dat2 <- setNames(Reduce(function(x, y) {
#'     merge(x, y, by = "word", all = TRUE)}, out2), c("Word", "A", "THE"))
#' 
#' dat2 <- reshape2::melt(dat2, id="Word", variable.name="Article", value.name="freq")
#' 
#' dat2 <- dat2[order(dat2$freq, dat2$Word), ]
#' 
#' ord2 <- aggregate(freq ~ Word, dat2, sum)
#' 
#' dat2$Word <- factor(dat2$Word, levels=ord2[order(ord2[[2]]), 1])
#' rownames(dat2) <- NULL
#' ggplot(dat2, aes(x=freq, y=Word)) +
#'     geom_point()+ facet_grid(~Article) +
#'     ggtitle("Part Of Speech Parsing Approach")
#' 
#' dev.new()
#' 
#' ## --------------------##
#' ## Regular Expressions ##
#' ## --------------------##
#' 
#' library(qdapRegex);library(ggplot2);library(reshape2)
#' 
#' out <- setNames(lapply(c("@@after_a", "@@after_the"), function(x) {
#'     o <- rm_default(stringi:::stri_trans_tolower(raj$dialogue),
#'         pattern = x, extract=TRUE)
#'     m <- stats::setNames(data.frame(sort(table(unlist(o))), 
#'         stringsAsFactors = FALSE), c("word", "freq"))
#'     m[m$freq> 3, ]
#' }), c("a", "the"))
#' 
#' dat <- setNames(Reduce(function(x, y) {
#'     merge(x, y, by = "word", all = TRUE)}, out), c("Word", "A", "THE"))
#' 
#' dat <- reshape2::melt(dat, id="Word", variable.name="Article", value.name="freq")
#' 
#' dat <- dat[order(dat$freq, dat$Word), ]
#' 
#' ord <- aggregate(freq ~ Word, dat, sum)
#' 
#' dat$Word <- factor(dat$Word, levels=ord[order(ord[[2]]), 1])
#' rownames(dat) <- NULL
#' ggplot(dat, aes(x=freq, y=Word)) + 
#'     geom_point()+ facet_grid(~Article) + 
#'     ggtitle("Regex Approach")
#' }
pos <-
function(text.var, parallel = FALSE, cores = detectCores()/2, 
    progress.bar = TRUE, na.omit = FALSE, digits = 1, percent = TRUE, 
    zero.replace=0, gc.rate=10){
        
    text.var <- strip(text.var, apostrophe.remove = FALSE)
    text.var[text.var == ""] <- NA

    if (parallel){
        ntv <- length(text.var)
        cl <- makeCluster(mc <- getOption("cl.cores", cores))
        clusterExport(cl=cl, varlist=c("text.var", "ntv", "gc.rate", 
            "tagPOS"), envir = environment())        
        ## clusterEvalQ(cl, {require(NLP); require(openNLP)})
        m <- parLapply(cl, seq_len(ntv), function(i) {
                x <- tagPOS(text.var[i])
                if (i%%gc.rate==0) gc()
                return(x)
            }
        ) 
        stopCluster(cl)
    } else { 

        pta <- Maxent_POS_Tag_Annotator()
        if (progress.bar){
            ntv <- length(text.var)
            if (Sys.info()[['sysname']] == "Windows" & progress.bar != "text"){
                pb <- utils::winProgressBar(title = "progress bar", min = 0,
                    max = ntv, width = 300)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- tagPOS(text.var[i], pta)
                        utils::setWinProgressBar(pb, i, title = paste(round(i/ntv*100, 0),
                            "% done"))
                        x
                    }
                )
                close(pb)
            } else {
                pb <- utils::txtProgressBar(min = 0, max = ntv, style = 3)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- tagPOS(text.var[i], pta)
                        utils::setTxtProgressBar(pb, i)
                        x
                    }
                )
                close(pb)
            }
        } else {
            m <- lapply(text.var, tagPOS, pta) 
        }
    }
    
    m2 <- data.frame(POStagged = unlist(lapply(m, "[[", 1)), stringsAsFactors = FALSE)
    m2$POStags <- lapply(m, "[[", 2)

    G4 <- mtabulate(m2$POStags)
#    m2$word.count <- wc(text.var)  
##   switched to apostrophe as word on 1/24/15
    m2$word.count <- sapply(m2$POStags, function(x){
        if (length(x) == "1" && (is.na(x) | grepl("^\\s+$", x))) return(NA)
        length(x)
    })
    cons <- ifelse(percent, 100, 1)

    G5 <- sapply(data.frame(G4, check.names = FALSE, stringsAsFactors = FALSE), 
        function(x) cons*(x/m2$word.count))
    ## Added data.frame wrap on 128-per Kurt Hornik's bug find
    if (is.vector(G5)) {
        G5 <- data.frame(t(G5), stringsAsFactors = FALSE)
    }
    colnames(G5) <- paste0("prop", colnames(G5))
    G4 <- data.frame(wrd.cnt = m2$word.count, G4, check.names = FALSE, stringsAsFactors = FALSE)
    G5 <- data.frame(wrd.cnt = m2$word.count, G5, check.names = FALSE, stringsAsFactors = FALSE)
    if (any(is.na(G4$wrd.cnt))) {
        nas <- which(is.na(G4$wrd.cnt))
        G4[nas, 2:ncol(G4)] <- NA
        m2[nas, 1:ncol(m2)] <- NA
    }

    rnp <- raw_pro_comb(G4[, -1, drop = FALSE], G5[, -1, drop = FALSE], 
        digits = digits, percent = percent, zero.replace = zero.replace, 
        override = TRUE)  
    rnp <- data.frame(G4[, 1, drop = FALSE], rnp, check.names = FALSE, stringsAsFactors = FALSE)     
    POS <- list(text = text.var, POStagged = m2, POSprop = G5, POSfreq = G4,
        POSrnp = rnp, percent = percent, zero.replace = zero.replace)
    if(na.omit) POS <- lapply(POS, na.omit)
    class(POS) <- "pos"
    POS
}

tagPOS <-  function(text.var, PTA, ...) {

    if (is.na(text.var)) return(list(POStagged = NA, POStags = NA))

    s <- as.String(text.var)

    ## Set up the POS annotator if missing (for parallel)
    if (missing(PTA)) {
        PTA <- Maxent_POS_Tag_Annotator()
    }

    ## Need sentence and word token annotations.
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- Annotation(1L, "sentence", 1L, nchar(s))
    a2 <- NLP::annotate(s, word_token_annotator, a2)
    a3 <- NLP::annotate(s, PTA, a2)

    ## Determine the distribution of POS tags for word tokens.
    a3w <- a3[a3$type == "word"]
    POStags <- unlist(lapply(a3w$features, "[[", "POS"))

    ## Extract token/POS pairs (all of them): easy.
    POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
    list(POStagged = POStagged, POStags = POStags)
}


#' Parts of Speech by Grouping Variable(s)
#' 
#' \code{pos_by} - Apply part of speech tagger to transcript(s) by zero or more 
#' grouping variable(s).
#' 
#' @rdname pos
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots Other argument supplied to \code{pos}.
#' @return \code{pos_by} -  returns a list of 6: 
#' \item{text}{The original text} 
#' \item{POStagged}{The original words replaced with parts of speech in context.} 
#' \item{POSprop}{Dataframe of the proportion of parts of speech by row.} 
#' \item{POSfreq}{Dataframe of the frequency of parts of speech by row.} 
#' \item{POSrnp}{Dataframe of the frequency and proportions of parts of speech 
#' by row.} 
#' \item{pos.by.prop}{Dataframe of the proportion of parts of speech by grouping 
#' variable.} 
#' \item{pos.by.freq}{Dataframe of the frequency of parts of speech by grouping 
#' variable.} 
#' \item{pos.by.rnp}{Dataframe of the frequency and proportions of parts of 
#' speech by grouping variable.} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @export
pos_by <-
function(text.var, grouping.var = NULL, digits = 1, percent = TRUE, 
    zero.replace = 0, ...){
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
 #   check <- FALSE
    if (any(class(text.var) %in% c("pos", "pos_by", "formality"))) {
        pos.list <- text.var
        text.var <- text.var[["POSfreq"]]
#       check <- TRUE
    } else {
        pos.list <- pos(text.var, digits = digits, percent = percent, ...)
        text.var <- pos.list[["POSfreq"]]
    }
    if(is.null(grouping.var)){
#        if (check) {
## commented out rows removed 1-29-14 to deal with single length vector
            grouping <- rep("all", nrow(text.var))  
#        } else {
#            grouping <- rep("all", length(text.var))
#        }
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF1 <- data.frame(grouping, text.var, check.names = FALSE, stringsAsFactors = FALSE)
    L1 <- split(DF1, DF1$grouping)
    L2 <- lapply(L1, function(x) colSums(x[, -1], na.rm = TRUE))
    DF2 <- data.frame(do.call("rbind", L2), check.names = FALSE, stringsAsFactors = FALSE)
    DF2 <- data.frame(replace = rownames(DF2), DF2, check.names = FALSE, stringsAsFactors = FALSE)
    rownames(DF2) <- 1:nrow(DF2)
    colnames(DF2)[1] <- G
    o <- unclass(pos.list)
    o[["pos.by.freq"]] <- DF2
    cons <- ifelse(percent, 100, 1)    
    propby <- lapply(1:nrow(DF2), function(i) {
        cons*(DF2[i, -c(1:2)]/rowSums(DF2[, -c(1:2)])[i])
    })
    propby <- as.matrix(do.call(rbind, propby))
    propby[is.nan(propby)] <- 0
    o[["pos.by.prop"]] <- suppressWarnings(data.frame(DF2[, 1:2], propby, 
        check.names = FALSE, stringsAsFactors = FALSE))
    rnp2 <- raw_pro_comb(o[["pos.by.freq"]][, -c(1:2)], 
        o[["pos.by.prop"]][, -c(1:2)], digits = digits, 
        percent = percent, zero.replace = zero.replace, override = TRUE)  
    o[["pos.by.rnp"]] <- data.frame(o[["pos.by.freq"]][, 1:2], 
        rnp2, check.names = FALSE, stringsAsFactors = FALSE)     
    class(o) <- "pos_by"
    attributes(o)[["grouping.var"]] <- DF1[["grouping"]]
    return(o)
}

#' Parts of Tags
#' 
#' \code{pos_tags} - Useful for interpreting the parts of speech tags created by 
#' pos and pos_by.
#' 
#' @rdname pos
#' @param type An optional character string giving the output of the pos tags.  
#' This must be one of the strings \code{"pretty"} (a left justified version of 
#' the output optimized for viewing but not good for export),  \code{"matrix"} 
#' (a matrix version of the output), \code{"dataframe"}\\ \code{"df"} (a 
#' dataframe version of the output), \code{"all"} (a list of all three of the 
#' previous output types).
#' @export
pos_tags <-
function(type = "pretty"){

    POStags.matrix <- as.matrix(POStags.df)
    POStags <- left_just(POStags.df, 1:2)   
    x <- switch(type,
        pretty = POStags,
        matrix = POStags.matrix,
        df = POStags.df,
        dataframe = POStags.df,
        all = {list(POStags.df=POStags.df, 
                  POStags.matrix=POStags.matrix, POStags=POStags)},
        stop("incorrect type specified")
    )
    return(x)
}

POStags.df <- structure(list(Tag = c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", 
    "JJR", "JJS", "LS", "MD", "NN", "NNS", "NNP", "NNPS", "PDT", 
    "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", 
    "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", 
    "WRB"), Description = c("Coordinating conjunction", "Cardinal number", 
    "Determiner", "Existential there", "Foreign word", "Preposition or subordinating conjunction", 
    "Adjective", "Adjective, comparative", "Adjective, superlative", 
    "List item marker", "Modal", "Noun, singular or mass", "Noun, plural", 
    "Proper noun, singular", "Proper noun, plural", "Predeterminer", 
    "Possessive ending", "Personal pronoun", "Possessive pronoun", 
    "Adverb", "Adverb, comparative", "Adverb, superlative", "Particle", 
    "Symbol", "to", "Interjection", "Verb, base form", "Verb, past tense", 
    "Verb, gerund or present participle", "Verb, past participle", 
    "Verb, non-3rd person singular present", "Verb, 3rd person singular present", 
    "Wh-determiner", "Wh-pronoun", "Possessive wh-pronoun", "Wh-adverb"
    )), .Names = c("Tag", "Description"), row.names = c(NA, -36L), 
    comment = "http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html", 
    class = "data.frame")


#' Prints a pos Object.
#' 
#' Prints a pos object.
#' 
#' @param x The pos object
#' @param digits Integer values specifying the number of digits to be 
#' printed.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proportion.  If NULL uses the value from 
#' \code{\link[qdap]{termco}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{termco}}.  Only used if \code{label} is TRUE.
#' @param \ldots ignored
#' @method print pos
#' @export
print.pos <-
function(x, digits = 1, percent = NULL, zero.replace = NULL, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    if (!is.null(percent)) {
        if (percent != x$percent) {
            DF <- as.matrix(x$POSprop[, -c(1:2)])
            if (percent) {
                DF <- DF*100    
            } else {
                DF <-  DF/100
            }
            x$POSprop <- data.frame(x$POSprop[, 1:2], DF, check.names = FALSE, stringsAsFactors = FALSE) 
        }
    } else {
        percent <- x$percent 
    }
    if (is.null(zero.replace)) {
        zero.replace <- x$zero.replace
    }
    rnp <- raw_pro_comb(x$POSfreq[, -1, drop = FALSE], 
        x$POSprop[, -1, drop = FALSE], digits = digits, percent = percent, 
        zero.replace = zero.replace)  
    rnp <- data.frame(x$POSfreq[, 1, drop = FALSE], rnp, check.names = FALSE)     
    print(rnp)
    options(width=WD)
}

#' Prints a pos_by Object.
#' 
#' Prints a pos_by object.
#' 
#' @param x The pos_by object
#' @param digits Integer values specifying the number of digits to be 
#' printed.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proportion.  If NULL uses the value from 
#' \code{\link[qdap]{termco}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{termco}}.  Only used if \code{label} is TRUE.
#' @param \ldots ignored
#' @method print pos_by
#' @export
print.pos_by <-
function(x, digits = 1, percent = NULL, zero.replace = NULL, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    if (!is.null(percent)) {
        if (percent != x$percent) {
            DF <- as.matrix(x$Pos.by.prop[, -c(1:2)])
            if (percent) {
                DF <- DF*100    
            } else {
                DF <-  DF/100
            }
            x$pos.by.prop <- data.frame(x$pos.by.prop[, 1:2], DF, 
                check.names = FALSE, stringsAsFactors = FALSE) 
        }
    } else {
        percent <- x$percent 
    }
    if (is.null(zero.replace)) {
        zero.replace <- x$zero.replace
    }
    rnp <- raw_pro_comb(x$pos.by.freq[, -c(1:2), drop = FALSE], 
        x$pos.by.prop[, -c(1:2), drop = FALSE], digits = digits, 
        percent = percent, zero.replace = zero.replace)  
    rnp <- data.frame(x$pos.by.freq[, 1:2, drop = FALSE], rnp, 
        check.names = FALSE, stringsAsFactors = FALSE)     
    print(rnp)
    options(width=WD)
}



#' Plots a pos_by Object
#' 
#' Plots a pos_by object.
#' 
#' @param x The pos_by object
#' @param label logical.  If TRUE the cells of the heat map plot will be labeled 
#' with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is TRUE.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proportion.  If NULL uses the value from 
#' \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param \ldots Other arguments passed to qheat.
#' @method plot pos_by
#' @export
plot.pos_by <- function(x, label = FALSE, lab.digits = 1, percent = NULL, 
    zero.replace = NULL, ...) {
    if (label) {
        if (!is.null(percent)) {
            if (percent != x$percent) {
                DF <- as.matrix(x$pos.by.prop[, -c(1:2)])
                if (percent) {
                    DF <- DF*100    
                } else {
                    DF <-  DF/100
                }
                x$pos.by.prop <- data.frame(x$pos.by.prop[, 1:2], DF, 
                    check.names = FALSE, stringsAsFactors = FALSE) 
            }
        } else {
            percent <- x$percent 
        }
        if (is.null(zero.replace)) {
            zero.replace <- x$zero.replace
        }
        rnp <- raw_pro_comb(x$pos.by.freq[, -c(1:2)], x$pos.by.prop[, -c(1:2)], 
            digits = lab.digits, percent = percent, , override = TRUE,
            zero.replace = x$zero.replace)  
        rnp <- data.frame(x$pos.by.freq[, 1:2], rnp, check.names = FALSE, stringsAsFactors = FALSE) 
        qheat(x$pos.by.prop, values=TRUE, mat2 = rnp, ...)
    } else {
        qheat(x$pos.by.prop, ...)  
    }  
}

#==============
## methods

#' Parts of Speech
#' 
#' View pos counts.
#' 
#' pos Method for counts
#' @param x The \code{\link[qdap]{pos}} object.
#' @param \ldots ignored
#' @export
#' @method counts pos
counts.pos <- function(x, ...) {

    out <- x[["POSfreq"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "pos_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Parts of Speech
#' 
#' View \code{\link[qdap]{pos}} proportions.
#' 
#' pos Method for proportions
#' @param x The pos object.
#' @param \ldots ignored
#' @export
#' @method proportions pos
proportions.pos <- function(x, ...) {

    out <- x[["POSfreq"]]
    out[, -c(1)] <- out[, -c(1)]/rowSums(out[, -c(1)])
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "pos_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Parts of Speech
#' 
#' View pos preprocessed.
#' 
#' pos Method for preprocessed 
#' @param x The \code{\link[qdap]{pos}} object.
#' @param \ldots ignored
#' @export
#' @method preprocessed pos
preprocessed.pos <- function(x, ...) {

    out <- x[["POStagged"]]
    attributes(out) <- list(
            class = c("pos_preprocessed", class(out)),
            type = "pos_preprocessed",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Parts of Speech
#' 
#' View pos_by preprocessed.
#' 
#' pos_by Method for preprocessed
#' @param x The \code{\link[qdap]{pos_by}} object.
#' @param \ldots ignored
#' @export
#' @method preprocessed pos_by
preprocessed.pos_by <- function(x, ...) {

    out <- x[["POStagged"]]
    attributes(out) <- list(
            class = c("pos_preprocessed", class(out)),
            type = "pos_by_preprocessed",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Parts of Speech
#' 
#' View pos_by scores.
#' 
#' pos_by Method for scores
#' @param x The \code{\link[qdap]{pos_by}} object.
#' @param \ldots ignored
#' @export
#' @method scores pos_by
scores.pos_by <- function(x, ...) {

    out <- x[["pos.by.rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "pos_by_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Parts of Speech
#' 
#' View pos_by counts.
#' 
#' pos_by Method for counts
#' @param x The \code{\link[qdap]{pos_by}} object.
#' @param \ldots ignored
#' @export
#' @method counts pos_by
counts.pos_by <- function(x, ...) {

    out <- x[["pos.by.freq"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "pos_by_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Parts of Speech
#' 
#' View \code{\link[qdap]{pos_by}} proportions.
#' 
#' pos_by Method for proportions
#' @param x The pos_by object.
#' @param \ldots ignored
#' @export
#' @method proportions pos_by
proportions.pos_by <- function(x, ...) {

    out <- x[["pos.by.freq"]]
    out[, -c(1:2)] <- out[, -c(1:2)]/out[, 2]
 
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "pos_by_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Plots a pos Object
#' 
#' Plots a pos object.
#' 
#' @param x The pos object
#' @param \ldots ignored
#' @method plot pos
#' @export
plot.pos <- function(x, ...) {

    graphics::plot(counts(x), ...)

}

#' Plots a pos_preprocessed Object
#' 
#' Plots a pos_preprocessed object.
#' 
#' @param x The pos_preprocessed object.
#' @param \ldots ignored
#' @importFrom qdapTools %l% list_df2df
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip ylab theme theme_bw
#' @method plot pos_preprocessed
#' @export
plot.pos_preprocessed <- function(x, ...){ 

    POS <- Counts <- NULL
    
    dat <- stats::setNames(data.frame(sort(table(unlist(x[, "POStags"]))), 
        stringsAsFactors = FALSE), c("POS", "Counts"))
    dat[, "POS"] <- dat[, "POS"] %l%  pos_tags("dataframe")
    dat[, "POS"] <- factor(dat[, "POS"], levels=dat[, "POS"])

    Max <- max(dat[, "Counts"])

    ggplot2::ggplot(dat, ggplot2::aes(POS)) + 
        ggplot2::geom_bar(ggplot2::aes(weights=Counts)) + 
        ggplot2::coord_flip() + 
        ggplot2::ylab("Count") +
        ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,Max + Max*.05)) +
        theme_qdap()

}