#' Parts of Speech Tagging
#' 
#' \code{pos} - Apply part of speech tagger to transcript(s).
#' 
#' @param text.var The text variable
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may not mean a spead boost if you have one core or if 
#' the data set is smaller as the cluster takes time to create.
#' @param na.omit logical.  If TRUE missing values (]code{NA}) will be omitted.
#' @param digits integer indicating the number of decimal places (round) or 
#' significant digits (signif) to be used. Negative values are allowed
#' @param progress.bar logical.  If TRUE attempts to provide a OS appropriate 
#' progress bar.  If parallel is TRUE this argument is ignored.  Note that 
#' setting this argument to TRUE may slow down the function.
#' @param gc.rate An integer value.  This is a necessary argument because of a 
#' problem with the garbage collection in the openNLP function that \code{pos} 
#' wraps.  Consider adjusting this argument upward if the error 
#' \code{java.lang.OutOfMemoryError} occurs.
#' @return pos returns a list of 4: 
#' \item{text}{The original text} 
#' \item{POStagged}{The original words replaced with parts of speech in context.} 
#' \item{POSprop}{Dataframe of the proportion of parts of speech by row.} 
#' \item{POSfreq}{Dataframe of the frequency of parts of speech by row.} 
#' @rdname pos
#' @seealso 
#' \code{\link[openNLP]{tagPOS}}
#' @references openNLP \url{http:/opennlp.apache.org}
#' @keywords parts-of-speech
#' @export
#' @import openNLP parallel openNLPmodels.en Snowball
#' @examples 
#' \dontrun{
#' posdat <- pos(DATA$state)
#' str(posdat)
#' names(posdat)
#' posdat$text       #original text
#' posdat$POStagged  #words replaced with parts of speech
#' posdat$POSprop    #proportion of parts of speech by row
#' posdat$POSfreq    #frequency of parts of speech by row
#' 
#' pos(DATA$state, parallel = TRUE) # not always useful
#' 
#' #use pos.tags to interpret part of speech tags used by pos & pos.by
#' pos.tags()
#' pos.tags("matrix")
#' pos.tags("dataframe")
#' pos.tags("df")
#' pos.tags("all")
#' 
#' posbydat <- with(DATA, pos.by(state, sex))
#' names(posbydat)
#' posbydat
#' posbydat$pos.by.prop
#' (POSby <- with(DATA, pos.by(state, list(adult, sex))))
#' plot(POSby, values = TRUE, digits = 2)
#' #or more quickly - reuse the output from before
#' with(DATA, pos.by(posbydat, list(adult, sex)))
#' }
pos <-
function(text.var, parallel = FALSE, na.omit = FALSE, digits = 2, 
    progress.bar = TRUE, gc.rate=10){
    ntv <- length(text.var)    
    pos1 <-  function(i) {
        x <- tagPOS(strip(i))   
        return(x)
    }
    if (parallel){
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "ntv", "gc.rate", 
            "pos1"), envir = environment())
        m <- parLapply(cl, seq_len(ntv), function(i) {
                x <- pos1(text.var[i])
                if (i%%gc.rate==0) gc()
                return(x)
            }
        )
        stopCluster(cl)
        m <- unlist(m)
    } else {
        if (progress.bar != FALSE){
            if (Sys.info()[['sysname']] == "Windows" & progress.bar != "text"){
                pb <- winProgressBar(title = "progress bar", min = 0,
                    max = ntv, width = 300)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- pos1(text.var[i])
                        if (i%%gc.rate==0) gc()
                        setWinProgressBar(pb, i, title = paste(round(i/ntv*100, 0),
                            "% done"))
                        return(x)
                    }
                )
                close(pb)
                m <- unlist(m)
            } else {
                pb <- txtProgressBar(min = 0, max = ntv, style = 3)
                m <- lapply(seq_len(ntv), function(i) {
                        x <- pos1(text.var[i])
                        if (i%%gc.rate==0) gc()
                        setTxtProgressBar(pb, i)
                        return(x)
                    }
                )
                close(pb)
                m <- unlist(m)
            }
        } else {
            m <- lapply(seq_len(ntv), function(i) {
                    x <- pos1(text.var[i])
                    if (i%%gc.rate==0) gc()
                    return(x)
                }
            )
            m <- unlist(m)
        }
    }
    names(m) <- NULL
    poser <- function(x) sub("^.*/([^ ]+).*$","\\1", 
        unlist(strsplit(x, " ")))
    o <- lapply(m, poser)
    lev <- sort(unique(unlist(o)))
    G4 <- do.call(rbind,lapply(o,function(x,lev){ 
            tabulate(factor(x,levels = lev, ordered = TRUE),
            nbins = length(lev))},lev = lev))
    colnames(G4) <-sort(lev)
    m <- data.frame(POStagged = m)
    m$POStags <- o 
    m$word.count <- word.count(text.var)
    G5 <- sapply(data.frame(G4, check.names = FALSE), 
        function(x) round(x/m$word.count, digits = digits)) 
    colnames(G5) <- paste0("prop", colnames(G5))
    G4 <- data.frame(wrd.cnt = m$word.count, G4, check.names = FALSE)
    G5 <- data.frame(wrd.cnt = m$word.count, G5, check.names = FALSE)
    if (any(is.na(G4$wrd.cnt))) {
        nas <- which(is.na(G4$wrd.cnt))
        G4[nas, 2:ncol(G4)] <- NA
        m[nas, 1:ncol(m)] <- NA
    }
    POS <- list(text = text.var, POStagged = m, POSprop = G5, POSfreq = G4)
    if(na.omit) POS <- lapply(POS, na.omit)
    class(POS) <- "pos"
    return(POS)
}

#' Parts of Speech by Grouping Variable(s)
#' 
#' \code{pos.by} - Apply part of speech tagger to transcript(s) by zero or more 
#' grouping variable(s).
#' 
#' @rdname pos
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param \ldots Other argument supplied to \code{pos}.
#' @return pos.by returns a list of 6: 
#' \item{text}{The original text} 
#' \item{POStagged}{The original words replaced with parts of speech in context.} 
#' \item{POSprop}{Dataframe of the proportion of parts of speech by row.} 
#' \item{POSfreq}{Dataframe of the frequency of parts of speech by row.} 
#' \item{pos.by.prop}{Dataframe of the proportion of parts of speech by grouping variable.} 
#' \item{pos.by.freq}{Dataframe of the frequency of parts of speech by grouping variable.} 
#' @export
pos.by <-
function(text.var, grouping.var = NULL, digits = 2, ...){
    G <- if(is.null(grouping.var)) {
             "all"
         } else {
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), 
                     function(x) x[length(x)])
                 paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    if (!class(text.var) %in% c("pos", "pos.by", "formality ")) {
        pos.list <- pos(text.var, digits = digits, ...)
        text.var <- pos.list[["POSfreq"]]
    } else {
        pos.list <- text.var
        text.var <- text.var[["POSfreq"]]
    }
    grouping <- if(is.null(grouping.var)){
                     rep("all", nrow(text.var))
                 } else {
                     if(is.list(grouping.var) & length(grouping.var)>1) {
                         apply(data.frame(grouping.var), 1, function(x){
                             if(any(is.na(x))){NA}else{paste(x, collapse = ".")
                                 }
                             }
                         )
                     } else {
                        unlist(grouping.var)
                     } 
                 } 
    DF1 <- data.frame(grouping, text.var, check.names = FALSE)
    L1 <- split(DF1, DF1$grouping)
    L2 <- lapply(L1, function(x) colSums(x[, -1], na.rm = TRUE))
    DF2 <- data.frame(do.call("rbind", L2), check.names = FALSE)
    DF2 <- data.frame(replace = rownames(DF2), DF2, check.names = FALSE)
    rownames(DF2) <- 1:nrow(DF2)
    colnames(DF2)[1] <- G
    o <- unclass(pos.list)
    o[["pos.by.freq"]] <- DF2
    propby <- lapply(1:nrow(DF2), function(i) DF2[i, -c(1:2)]/rowSums(DF2[, -c(1:2)])[i])
    propby <- sapply(do.call(rbind, propby), round, digits = digits)
    propby[is.nan(propby)] <- 0
    o[["pos.by.prop"]] <- data.frame(DF2[, 1:2], propby, check.names = FALSE)
    class(o) <- "pos.by"
    return(o)
}

#' Parts of Tags
#' 
#' \code{pos.tags} - Useful for interpreting the parts of speech tags created by 
#' pos and pos.by.
#' 
#' @rdname pos
#' @param type An optional character string giving the output of the pos tags.  
#' This must be one of the strings \code{"pretty"} (a left justified version of 
#' the output optimized for viewing but not good for export),  \code{"matrix"} 
#' (a matrix version of the output), \code{"dataframe"}\\ \code{"df"} (a 
#' dataframe varseion of the output), \code{"all"} (a list of all three of the 
#' previous output types).
#' @export
pos.tags <-
function(type = "pretty"){
        POStags.df <- structure(list(Tag = structure(c(1L, 
            2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 
            13L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 
            24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 
            35L, 36L), .Label = c("CC", "CD", "DT", "EX", "FW", 
            "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNP", 
            "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB",
            "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", 
            "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"), 
            class = "factor"), Description = structure(c(8L, 7L, 
            9L, 10L, 11L, 23L, 1L, 2L, 3L, 13L, 14L, 16L, 15L, 
            25L, 24L, 22L, 19L, 18L, 20L, 4L, 5L, 6L, 17L, 26L, 
            27L, 12L, 29L, 33L, 30L, 32L, 31L, 28L, 35L, 36L, 
            21L, 34L), .Label = c("Adjective", 
            "Adjective, comparative", "Adjective, superlative", 
            "Adverb", "Adverb, comparative", "Adverb, superlative", 
            "Cardinal number", "Coordinating conjunction", 
            "Determiner", "Existential there", "Foreign word", 
            "Interjection", "List item marker", "Modal", 
            "Noun, plural", "Noun, singular or mass", "Particle", 
            "Personal pronoun", "Possessive ending", 
            "Possessive pronoun", "Possessive wh-pronoun", 
            "Predeterminer", 
            "Preposition or subordinating conjunction", 
            "Proper noun, plural", "Proper noun, singular", 
            "Symbol", "to", "Verb, 3rd person singular present", 
            "Verb, base form", "Verb, gerund or present participle", 
            "Verb, non-3rd person singular present", 
            "Verb, past participle", "Verb, past tense", "Wh-adverb", 
            "Wh-determiner", "Wh-pronoun"), class = "factor")), 
            .Names = c("Tag", "Description"), row.names = c(NA, -36L), 
            class = "data.frame", comment = 
            "http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html")
    POStags.matrix <- as.matrix(POStags.df)
    POStags <- left.just(POStags.df, 1:2)   
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


#' Prints a pos Object.
#' 
#' Prints a pos object.
#' 
#' @param x The pos object
#' @param \ldots ignored
#' @method print pos
#' @S3method print pos
print.pos <-
function(x, ...) {
    print(x$POSfreq)
}

#' Prints a pos.by Object.
#' 
#' Prints a pos.by object.
#' 
#' @param x The pos.by object
#' @param \ldots ignored
#' @method print pos.by
#' @S3method print pos.by
print.pos.by <-
function(x, ...) {
    print(x$pos.by.freq)
}

#' Plots a pos.by Object
#' 
#' Plots a pos.by object.
#' 
#' @param x The pos.by object
#' @param \ldots Other arguments passed to qheat.
#' @method plot pos.by
#' @S3method plot pos.by
plot.pos.by <- function(x, ...) {
    qheat(x$pos.by.prop, ...)
}