#' Word Frequency Matrix
#' 
#' \code{wfm} - Generate a word frequency matrix by grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param output Output type (either \code{"proportion"} or \code{"percent"}).
#' @param stopwords A vector of stop words to remove.
#' @param char2space A vector of characters to be turned into spaces.  If 
#' \code{char.keep} is \code{NULL}, \code{char2space} will activate this 
#' argument.
#' @param \ldots Other arguments supplied to \code{\link[tm]{Corpus}} or
#' \code{\link[tm]{TermDocumentMatrix}}.  If \code{as.wfm} this is other 
#' arguments passed to \code{as.wfm} methods (currently ignored).
#' @param digits An integer indicating the number of decimal places (round) or 
#' significant digits (signif) to be used. Negative values are allowed.
#' @param margins logical. If \code{TRUE} provides grouping.var and word 
#' variable totals.
#' @param word.lists A list of character vectors of words to pass to 
#' \code{wfm_combine}
#' @param matrix logical.  If \code{TRUE} returns the output as a 
#' \code{\link[qdap]{wfm}} rather than a \code{\link[qdap]{wfdf}} object.
#' @return \code{wfm} - returns a word frequency of the class matrix.
#' @rdname Word_Frequency_Matrix
#' @note Words can be kept as one by inserting a double tilde (\code{"~~"}), or 
#' other character strings passed to char2space, as a single word/entry. This is 
#' useful for keeping proper names as a single unit.
#' @keywords word-frequency-matrix
#' @export
#' @importFrom qdapTools mtabulate
#' @examples
#' \dontrun{
#' ## word frequency matrix (wfm) example:
#' with(DATA, wfm(state, list(sex, adult)))[1:15, ]
#' with(DATA, wfm(state, person))[1:15, ]
#' Filter(with(DATA, wfm(state, list(sex, adult))), 5)
#' with(DATA, wfm(state, list(sex, adult)))
#' 
#' ## Filter particular words based on max/min values in wfm
#' v <- with(DATA, wfm(state, list(sex, adult)))
#' Filter(v, 5)
#' Filter(v, 5, count.apostrophe = FALSE)
#' Filter(v, 5, 7)
#' Filter(v, 4, 4)
#' Filter(v, 3, 4)
#' Filter(v, 3, 4, stopwords = Top25Words)
#' 
#' ## insert double tilde ("~~") to keep phrases(i.e., first last name)
#' alts <- c(" fun", "I ")
#' state2 <- space_fill(DATA$state, alts, rm.extra = FALSE)
#' with(DATA, wfm(state2, list(sex, adult)))[1:18, ]
#' 
#' ## word frequency dataframe (wfdf) example:
#' with(DATA, wfdf(state, list(sex, adult)))[1:15, ]
#' with(DATA, wfdf(state, person))[1:15, ]
#' 
#' ## wfm_expanded example:
#' z <- wfm(DATA$state, DATA$person)
#' wfm_expanded(z)[30:45, ] #two "you"s
#' 
#' ## wf_combine examples:
#' #===================
#' ## raw no margins (will work) 
#' x <- wfm(DATA$state, DATA$person) 
#'                     
#' ## raw with margin (will work) 
#' y <- wfdf(DATA$state, DATA$person, margins = TRUE) 
#' 
#' ## Proportion matrix
#' z2 <- wfm(DATA$state, DATA$person, output="proportion")
#'
#' WL1 <- c(y[, 1])                                                                      
#' WL2 <- list(c("read", "the", "a"), c("you", "your", "you're"))                       
#' WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "you're"))          
#' WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))     
#' WL5 <- list(yous = c("you", "your", "your're"))                                       
#' WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1          
#' WL7 <- c("you", "your", "your're")                             
#'                                                                
#' wfm_combine(z2, WL2) #Won't work not a raw frequency matrix     
#' wfm_combine(x, WL2)  #Works (raw and no margins)                     
#' wfm_combine(y, WL2)  #Works (raw with margins)                           
#' wfm_combine(y, c("you", "your", "your're"))                        
#' wfm_combine(y, WL1)                                                  
#' wfm_combine(y, WL3)                                                   
#' ## wfm_combine(y, WL4) #Error         
#' wfm_combine(y, WL5)                                         
#' wfm_combine(y, WL6)                                              
#' wfm_combine(y, WL7)                                           
#'                                                                   
#' worlis <- c("you", "it", "it's", "no", "not", "we")              
#' y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)  
#' z <- wfm_combine(y, worlis)                      
#'                                                                  
#' chisq.test(z)                                                      
#' chisq.test(wfm(y)) 
#' 
#' ## Dendrogram
#' presdeb <- with(pres_debates2012, wfm(dialogue, list(person, time)))
#' library(sjPlot)
#' sjc.dend(t(presdeb), 2:4)
#' 
#' ## Words correlated within turns of talk
#' ## EXAMPLE 1
#' library(reports)
#' x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
#' dat <- wfm(rajSPLIT$dialogue, x)
#' 
#' cor(t(dat)[, c("romeo", "juliet")])
#' cor(t(dat)[, c("romeo", "banished")])
#' cor(t(dat)[, c("romeo", "juliet", "hate", "love")])
#' qheat(cor(t(dat)[, c("romeo", "juliet", "hate", "love")]), 
#'     diag.na = TRUE, values = TRUE, digits = 3, by.column = NULL)
#'     
#' dat2 <- wfm(DATA$state, id(DATA))
#' qheat(cor(t(dat2)), low = "yellow", high = "red", 
#'     grid = "grey90", diag.na = TRUE, by.column = NULL)
#'     
#' ## EXAMPLE 2
#' x2 <- factor(with(pres_debates2012, paste(time, pad(TOT(tot)), sep = "|")))
#' dat2 <- wfm(pres_debates2012$dialogue, x2)
#' wrds <- word_list(pres_debates2012$dialogue, 
#'     stopwords = c("it's", "that's", Top200Words))
#' wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
#' qheat(word_cor(t(dat2), word = wrds2, r = NULL),
#'     diag.na = TRUE, values = TRUE, digits = 3, by.column = NULL, 
#'     high="red", low="yellow", grid=NULL)
#'     
#' ## EXAMPLE 3
#' library(gridExtra); library(ggplot2); library(grid)
#' dat3 <- lapply(qcv(OBAMA, ROMNEY), function(x) {
#'     with(pres_debates2012, wfm(dialogue[person == x], x2[person == x]))
#' })
#' 
#' 
#' # Presidential debates by person
#' dat5 <- pres_debates2012
#' dat5 <- dat5[dat5$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' disp <- with(dat5, dispersion_plot(dialogue, wrds2, grouping.var = person, 
#'     total.color = NULL, rm.vars=time))
#' 
#' 
#' cors <- lapply(dat3, function(m) {
#'     word_cor(t(m), word = wrds2, r = NULL)
#' })
#' 
#' plots <- lapply(cors, function(x) {
#'     qheat(x, diag.na = TRUE, values = TRUE, digits = 3, plot = FALSE,
#'     by.column = NULL, high="red", low="yellow", grid=NULL)
#' })
#' 
#' plots <- lapply(1:2, function(i) {
#'     plots[[i]] + ggtitle(qcv(OBAMA, ROMNEY)[i]) +
#'     theme(axis.title.x = element_blank(),
#'         plot.margin = unit(rep(0, 4), "lines"))
#' })
#' 
#' grid.arrange(disp, arrangeGrob(plots[[1]], plots[[2]], ncol=1), ncol=2)
#' 
#' ## With `word_cor`
#' worlis <- list(
#'     pronouns = c("you", "it", "it's", "we", "i'm", "i"),
#'     negative = qcv(no, dumb, distrust, not, stinks),
#'     literacy = qcv(computer, talking, telling)
#' )
#' y <- wfdf(DATA$state, qdapTools::id(DATA, prefix = TRUE))
#' z <- wfm_combine(y, worlis)
#' 
#' word_cor(t(z), word = names(worlis), r = NULL)
#' 
#' ## Plotting method
#' plot(y, TRUE)
#' plot(z)
#' 
#' ## Correspondence Analysis
#' library(ca)
#' 
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' speech <- stemmer(dat$dialogue)
#' mytable1 <- with(dat, wfm(speech, list(person, time), stopwords = Top25Words))
#' 
#' fit <- ca(mytable1)
#' summary(fit)
#' plot(fit)
#' plot3d.ca(fit, labels=1)
#' 
#' 
#' mytable2 <- with(dat, wfm(speech, list(person, time), stopwords = Top200Words))
#' 
#' fit2 <- ca(mytable2)
#' summary(fit2)
#' plot(fit2)
#' plot3d.ca(fit2, labels=1)
#' 
#' ## Weight a wfm
#' WFM <- with(DATA, wfm(state, list(sex, adult)))
#' plot(weight(WFM, "scaled"), TRUE)
#' weight(WFM, "prop")
#' weight(WFM, "max")
#' weight(WFM, "scaled")
#' }
wfm <- function(text.var = NULL, grouping.var = NULL, output = "raw", 
    stopwords = NULL, char2space = "~~", ...){

    text.var
    grouping.var 
    output
    stopwords
    
    UseMethod("wfm")
} 

#' \code{wfm.wfdf} - wfdf method for \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method wfm wfdf    
wfm.wfdf <- 
function(text.var = NULL, grouping.var = NULL, output = "raw", stopwords = NULL, 
    char2space = "~~", ...){

    if (methods::is(text.var, "t.df")) {
        wfdf <- text.var
    } else {
        if (methods::is(text.var, "m.df")) { 
            wfdf <- text.var[-nrow(text.var), -ncol(text.var)]
        } else {
            stop("Object must be a raw word frequency data frame")
        }
    }
    x2 <- wfdf[, -1, drop = FALSE]
    rownames(x2) <- wfdf[, 1]
    x2 <- as.matrix(x2)

    class(x2) <- c("wfm", "true.matrix", class(x2))
    x2

}

#' \code{wfm.character} - character method for \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method wfm character 
wfm.character <- 
function(text.var = NULL, grouping.var = NULL, output = "raw", stopwords = NULL, 
    char2space = "~~", ...){

    if(is.null(stopwords)) stopwords <- FALSE
    tm_tdm_interface(text.var = text.var, grouping.var = grouping.var, 
        output = output, stopwords = stopwords, char2space = char2space, ...)
}

#' \code{wfm.factor} - factor method for \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method wfm factor 
wfm.factor <- wfm.character

## SAVE historical reasons
##
## ## more flexible slower wfm helper
## wfm_flexible <- function(text.var, grouping.var, output, stopwords, 
##     char2space, ...){
## 
##     if(is.null(grouping.var)){
##         grouping <- rep("all", length(text.var))
##     } else {
##         if (is.list(grouping.var) & length(grouping.var)>1) {
##             grouping <- paste2(grouping.var)
##         } else {
##             grouping <- unlist(grouping.var)
##         } 
##     } 
##     txt <- strip(text.var, char.keep = char2space, 
##         apostrophe.remove = FALSE, ...)
##     txtL <- lapply(split(txt, grouping), function(x) {
##           unlist(strsplit(x, "\\s+"))
##     })
## 
##     ## tabulate frequencies per word
##     x2 <- t(mtabulate(txtL))
## 
##     ## replace spaced characters
##     if (!is.null(char2space)) {
##         rownames(x2) <- mgsub(char2space, " ", rownames(x2))
##     } 
## 
##     if (!is.null(stopwords)){
##         x2 <- x2[!rownames(x2) %in% tolower(stopwords), , drop = FALSE]
##     }
##     if (output != "raw"){
##         x2 <- x2/colSums(x2)
##         if (output == "percent") {
##             x2 <- x2*100
##         }
##         class(x2) <- c("wfm", "prop.matrix", class(x2))
##         return(x2)
##     }
## 
##     class(x2) <- c("wfm", "true.matrix", class(x2))
##     x2
## }
##
## SAVE historical reasons

## less flexible faster wfm helper
tm_tdm_interface <- function(text.var, grouping.var, stopwords, char2space, 
    output = output, apostrophe.remove, ...){

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
    
    # ## Use the tm package to convert to a Corpus
    # mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF), 
    #     readerControl=list(reader=qdap_tm_reader))
    mycorpus <- tm::VCorpus(tm::DataframeSource(LST_DF))
    
    ## Add metadata info
    NLP::meta(mycorpus, "MetaID") <- names(LST)
    NLP::meta(mycorpus, "labels") <- names(LST)
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        tm::DublinCore(mycorpus, tag = "creator") <- pers
    }

    if(missing(apostrophe.remove)) apostrophe.remove <- FALSE

    apo_rm <- TRUE

    if(!apostrophe.remove) {
        apo_rm <- function(x) gsub(paste0(".*?($|'|", paste(paste0("\\", 
            char2space), collapse = "|"), "|[^[:punct:]]).*?"), 
            "\\1", x)
    }

    pdots <- eval(substitute(list(...)))
    
    if(is.null(pdots[["removeNumbers"]])) {
        rmnum <- TRUE
    } else {
        rmnum <- pdots[["removeNumbers"]]
    }
    
    m <- as.wfm(tm::TermDocumentMatrix(mycorpus,
        control = list(
            removePunctuation = apo_rm,
            wordLengths =c(1, Inf),
            stopwords = stopwords,
            removeNumbers = rmnum, ...
        )
    ))
    colnames(m) <- names(LST)
    rownames(m) <- mgsub(char2space, " ", rownames(m))
    m <- m[rownames(m) != "", ]

    if (!is.matrix(m)) {
        m <- as.matrix(m)
        colnames(m) <- G
    }
    if (output != "raw"){
        m <- m/colSums(m)
        if (output == "percent") {
            m <- m*100
        }
        class(m) <- gsub("true.matrix", "prop.matrix", class(m))
        return(m)
    }    
    class(m) <- c("wfm", "true.matrix", class(m))
    m
}




#' Prints a wfm Object
#' 
#' Prints a wfm object.
#' 
#' @param x The wfm object.
#' @param width  The width to temporarily set for printing (default = 10000).  
#' See \code{\link[base]{options}} for more.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print wfm
#' @export
print.wfm <-
  function(x, digits = 3, width = 10000, ...) {
    class(x) <- "matrix"
      
    WD <- options()[["width"]]
    if (!is.null(width)) {
        options(width=width)
    }
    print(round(x, digits = digits))
    options(width=WD)
      
}


#' Word Frequency Data Frame
#' 
#' \code{wfdf} - Generate a word frequency data frame by grouping variable.
#' 
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wfdf} - returns a word frequency of the class data.frame with 
#' a words column and optional margin sums.
wfdf <-
function(text.var, grouping.var = NULL, stopwords = NULL,
    margins = FALSE, output = "raw", digits = 2, char2space = "~~", ...){
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    bl <- split(text.var, grouping)
    x <- lapply(bl, bag_o_words, char.keep = char2space, ...)
    tabs <- lapply(x, function(x) as.data.frame(table(x)))
    tabs <- tabs[sapply(tabs, nrow)!=0]
    lapply(seq_along(tabs), function(x) {
        names(tabs[[x]]) <<- c("Words", names(tabs)[x])  
    }) 
    DF <- merge_all(tabs, by="Words", 0)
    DF <- DF[order(DF$Words), ]
    DF[, "Words"] <- as.character(DF[, "Words"])
    DF[, -1] <- sapply(DF[, -1], function(x) as.numeric(as.character(x)))
    if(!is.null(stopwords)) DF <- DF[!DF[, "Words"] %in% stopwords , ]
    rownames(DF) <- 1:nrow(DF)
    pro <- function(x) x/sum(x)       #helper function 1
    per <- function(x) 100*(x/sum(x)) #helper function 2     
    if (output != "raw") DF2 <- DF
    DF <- switch(output,
         raw = DF,
         proportion = {data.frame(DF[, 1, drop = FALSE], 
             sapply(DF[, -1], pro))},
         prop = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], pro)),
         percent = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], per)),
         per = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], per))
    )
    if (margins){
        if (output == "raw"){
            DF <- rbind(DF, c(NA, colSums(DF[, -1])))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
            DF[, "TOTAL.USES"] <- rowSums(DF[, -1])
        } else {
            X <- rowSums(DF2[, -1])
            DF[, "TOTAL.USES"] <- c(X/sum(X))   
            X2 <- colSums(DF2[, -1])  
            DF <- rbind(DF, c(NA, X2/sum(X), 1))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
        }
    }
    if (!output == "raw") {
        DF2 <- lapply(DF[, -1], function(x) round(x, digits = digits))
        DF <- data.frame(DF[, 1, drop = FALSE], DF2)
    }
    if (!margins & output == "raw") {
        class(DF) <- c("t.df", class(DF)) 
    } else {
            if (margins & output == "raw") {
                class(DF) <- c("m.df", class(DF))
            } else {
                class(DF) <- c("f.df", class(DF))
        }
    }
    if (!is.null(char2space)) {
        DF[, "Words"] <- mgsub(char2space, " ", DF[, "Words"])
    }
    class(DF) <- c("wfdf", class(DF))
    DF
}

#' Expanded Word Frequency Matrix
#' 
#' \code{wfm_expanded} - Expand a word frequency matrix to have multiple rows 
#' for each word.
#' 
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wfm_expanded} - returns a matrix similar to a word frequency 
#' matrix (\code{wfm}) but the rows are expanded to represent the maximum usages 
#' of the word and cells are dummy coded to indicate that number of uses.
wfm_expanded <-
function(text.var, grouping.var = NULL, ...){
    if(methods::is(text.var, "true.matrix")) {
        z <- text.var
    } else {
        if(methods::is(text.var, "m.df")){
            z <- wfm(text.var)
        } else {
            z <- wfm(text.var, grouping.var, ...)
        }
    }
    rows <-lapply(1:nrow(z), function(i) z[i, ])
    names(rows) <- rownames(z)
    lens <- sapply(1:nrow(z), function(i) max(z[i, ]))
    rep(rownames(z), lens)
    repper <- function(R) {
        mx <- max(R)
        sapply(R, function(x) c(rep(1, x), rep(0, mx-x)))
    }
    expanded <- do.call(rbind, lapply(1:nrow(z), function(i) repper(z[i, ])))
    rownames(expanded) <- rep(rownames(z), lens)
    expanded
}


#' Combined Word Frequency Matrix Terms
#' 
#' \code{wfm_combine} - Combines words (rows) of a word frequency matrix 
#' (\code{wfdf}) together.
#'
#' @param wf.obj A \code{wfm} or \code{wfdf} object.
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wfm_combine} - returns a word frequency matrix (\code{wfm}) or 
#' dataframe (\code{wfdf}) with counts for the combined word.lists merged and 
#' remaining terms (\code{else}).
wfm_combine <- function(wf.obj, word.lists, matrix = TRUE){
    suppressWarnings(if (is.list(word.lists) & length(word.lists) > 1 & 
        any(Reduce("%in%", word.lists))) {
        stop("overlapping words in word.lists")
    })
    if (methods::is(wf.obj, "t.df")) {
        wf.obj <- wf.obj
    } else {
   
        if (methods::is(wf.obj, "m.df")) { 
            wf.obj <- wf.obj [-nrow(wf.obj), -ncol(wf.obj)]
        } else {
            if (!methods::is(wf.obj, "true.matrix")) {
                stop("Object must be a raw word frequency matrix/data.frame")
            }
        }
    }
    if (is.list(word.lists) & is.null(names(word.lists))){
        NAMES <- paste("words", 1:length(word.lists))
    } else {
        if (is.list(word.lists) & !is.null(names(word.lists))){
            NAMES <- names(word.lists)
        } else {
            if (is.vector(word.lists)) {
                    G <- as.character(substitute(word.lists))
                if (G[1] == "c") {
                    NAMES <- "words"
                } else {
                    NAMES <- G[length(G)]
                }
            } else {
                stop("incorrect word.list argument")
            }
        }
    }
    if(!is.list(word.lists)) {
        word.lists <- list(word.lists)
    }
    if (methods::is(wf.obj, "true.matrix")) {
        wf.obj <- data.frame(rownames(wf.obj), wf.obj, check.names = FALSE)
    }
    j <- lapply(word.lists, function(x) wf.obj [wf.obj [, 1] %in% x, -1])
    if (!all(wf.obj [, 1] %in% unlist(word.lists))) {
        j[[length(j) + 1]] <- wf.obj [!wf.obj [, 1] %in% unlist(word.lists), -1]
    }
    k <- lapply(j, function(x) if(is.vector(x)) { x } else { colSums(x)})
    m <- do.call("rbind", k)
    rownames(m) <- 1:nrow(m)
    NAMES <- if (all(wf.obj[, 1] %in% unlist(word.lists))) {
        NAMES 
    } else {
        c(NAMES, "else.words")
    }
    DFF <- data.frame(word.group = NAMES, m, check.names = FALSE)
    if (matrix) {
        DFF2 <- as.matrix(DFF[, -1])
        rownames(DFF2) <- as.character(DFF[, 1])
        return(as.wfm(DFF2))
    }
    class(DFF) <- c("wfdf", class(DFF))
    DFF
}


#' Plots a wfm object
#' 
#' Plots a wfm object.
#' 
#' @param x The wfm object
#' @param non.zero logical.  If \code{TRUE} all values converted to dummy coded 
#' based on x_ij > 0.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param by.column logical.  If \code{TRUE} applies scaling to the column.  If 
#' \code{FALSE}  applies scaling by row (use \code{NULL} to turn off scaling).
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid).  
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param \ldots Other arguments passed to qheat.
#' @method plot wfm
#' @export
plot.wfm <- function(x, non.zero = FALSE, digits = 0, by.column = NULL,
    high = ifelse(non.zero, "black", "blue"),  
    grid = ifelse(non.zero, "black", "white"), plot = TRUE, ...) {

    class(x) <- "matrix"

    if (non.zero) {
        if(missing(by.column)) {
            by.column <- NULL
        }
        x <- data.frame(x)
        x[1:ncol(x)] <- lapply(x, function(z) as.numeric(z > 0))
    } else {
        if(missing(by.column)) {
            by.column <- FALSE
        }

    }

    out <- qheat(t(x), digits = digits, high=high, grid = grid,
        by.column = by.column, plot = FALSE, ...) 
 
    if (non.zero) {
        out <- out + guides(fill=FALSE)
    }
    if (plot) {
        print(out)
    }
    invisible(out)
}

#' Plots a wfdf object
#' 
#' Plots a wfdf object.
#' 
#' @param x The wfdf object
#' @param \ldots Other arguments passed to \code{\link[qdap]{plot.wfm}}.
#' @method plot wfdf
#' @export
plot.wfdf <- function(x, ...) {

    x <- wfm(x)
    plot.wfm(x, ...)

}

#' Summarize a wfm object
#' 
#' Summarize a wfm object with familiar tm package look.
#' 
#' @param object The wfm object 
#' @param \ldots Ignored.
#' @method summary wfm
#' @details \strong{Non-/sparse entries} is the ratio of non-zeros to zero 
#' counts.  \strong{Sparsity} is that ratio represented as a percent.  
#' \strong{Hapax legomenon} is the number(percent) of terms that appear only 
#' once in the dialogue. \strong{Dis legomenon} is the number(percent) of terms 
#' that appear exactly two times once.
#' @export
#' @examples
#' \dontrun{
#' x <- with(DATA, wfm(state, list(sex, adult)))
#' summary(x)
#' }
summary.wfm <- function(object, ...) {

    class(object) <- "matrix"
    x <- object

    B <- x!=0
    Y <- sum(B)
    N <- sum(!B)
    density <- Y/(N + Y)
    sparsity <- round(1 - density, 2)*100
    NCHAR <- nchar(rownames(x))
    RS <- rowSums(x)
    HL <- sum(RS == 1)
    DL <- sum(RS == 2)
    shan <- shannon(RS)             
    output <- list(
        c(Y, N),
        c(sparsity),
        c(max(NCHAR)),
        c(sum(NCHAR < 4)/nrow(x)),
        c(HL, HL/nrow(x)),
        c(DL, DL/nrow(x)),
        c(shan)
    )
    names(output) <- c("Non-/sparse entries", "Sparsity", 
        "Maximal term length", "Less than four characters", 
        "Hapax legomenon", "Dis legomenon", "Shannon's diversity index"                                          
    )        

    attributes(output) <- list(
            class = c("wfm_summary"),
            names = names(output),
            nrow = nrow(x),
            ncol = ncol(x)
    )       

    output

}


#' Prints a wfm_summary Object
#' 
#' Prints a wfm_summary object.
#' 
#' @param x The wfm_summary object.
#' @param \ldots ignored
#' @method print wfm_summary
#' @export
print.wfm_summary <- function(x, ...) {

    nms <- c("Non-/sparse entries", "Sparsity", 
        "Maximal term length", "Less than four characters", 
        "Hapax legomenon", "Dis legomenon", "Shannon's diversity index"                                          
    ) 

    numrow <- attributes(x)[["nrow"]]
    numcol <- attributes(x)[["ncol"]]
    class(x) <- "list"

    if (!all(nms %in% names(x))) {
        print(x)
        return(invisible(NULL))
    }      

    vals <- c(
        sprintf("<<A word-frequency matrix (%s terms, %s groups)>>", numrow, numcol),
        "", sprintf("Non-/sparse entries       : %s/%s", x[[nms[1]]][1], x[[nms[1]]][2]),
        sprintf("Sparsity                  : %s%%", x[[nms[2]]]),
        sprintf("Maximal term length       : %s", x[[nms[3]]]) ,
        sprintf("Less than four characters : %s%%", 100*round(x[[nms[4]]], 2)) ,
        sprintf("Hapax legomenon           : %s(%s%%)", x[[nms[5]]][1], 100*round(x[[nms[5]]][2], 2)),
        sprintf("Dis legomenon             : %s(%s%%)", x[[nms[6]]][1], 100*round(x[[nms[6]]][2], 2)),
        sprintf("Shannon's diversity index : %s\n", round(x[[nms[7]]], 2))
    )
    cat(paste(vals, collapse="\n"))
}

#' Summarize a wfdf object
#' 
#' Summarize a wfdf object with familiar tm package look.
#' 
#' @param object The wfdf object 
#' @param \ldots Ignored.
#' @details \strong{Non-/sparse entries} is the ratio of non-zeros to zero 
#' counts.  \strong{Sparsity} is that ratio represented as a percent.  
#' \strong{Hapax legomenon} is the number(percent) of terms that appear only 
#' once in the dialogue. \strong{Dis legomenon} is the number(percent) of terms 
#' that appear exactly two times once.
#' @method summary wfdf
#' @export
#' @examples
#' \dontrun{
#' x <- with(DATA, wfdf(state, list(sex, adult)))
#' summary(x)
#' }
summary.wfdf <- function(object, ...) {

    summary.wfm(wfm(object))

}

#' Weighted Word Frequency Matrix
#' 
#' \code{weight} - Weight a word frequency matrix for analysis where such 
#' weighting is sensible.
#' 
#' @param type The type of weighting to use: c(\code{"prop"}, \code{"max"}, 
#' \code{"scaled"}).  All weight by column.  \code{"prop"} uses a proportion
#' weighting and all columns sum to 1.  \code{"max"} weights in proportion to 
#' the max value; all values are integers and column sums may not be equal.
#' \code{"scaled"} uses \code{\link[base]{scale}} to scale with 
#' \code{center = FALSE}; output is not integer and column sums may not be 
#' equal.
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{weight} - Returns a weighted matrix for use with other R 
#' packages. The output is not of the class "wfm".
#' @export
#' @method weight wfm
weight.wfm <- function(x, type = "prop", ...) {

    if (methods::is(x, "wfdf") && !methods::is(x, "f.df")) {
        x <- wfm(x)
    }
  
    types <- c("prop", "max", "scaled")

    if (is.numeric(type)) {
        type <- types[type]
    }

    switch(type,
        prop = {FUN <- function(x) apply(x, 2, function(y) y/sum(y))},
        max = {FUN <- function(x) apply(x, 2, function(y) round(y *(max(x)/max(y)), 0))},
        scaled = {FUN <- function(x) {
                o <- apply(x, 2, function(y) scale(y, FALSE))
                rownames(o) <- rownames(x)
                o
            }} ,
        stop("`type` must be one of c(\"prop\", \"max\", \"scaled\")")
    )

    out <- FUN(x)
    class(out) <- c("weighted_wfm", class(out))
    attributes(out)[["weighting"]] <- type

    out
}

#' Weighted Word Frequency Matrix
#' 
#' \code{weight.wfdf} - Weight a word frequency matrix for analysis where such 
#' weighting is sensible.
#' 
#' @rdname Word_Frequency_Matrix
#' @export
#' @method weight wfm
weight.wfdf <- function(x, type = "prop", ...) {

    if (methods::is(x, "wfdf") && !methods::is(x, "f.df")) {
        x <- wfm(x)
    } else {
        stop(paste("no applicable method for 'weight' applied to an object of", "class \"wfdf\" that is proportional"))
    }
  
    types <- c("prop", "max", "scaled")

    if (is.numeric(type)) {
        type <- types[type]
    }

    switch(type,
        prop = {FUN <- function(x) apply(x, 2, function(y) y/sum(y))},
        max = {FUN <- function(x) apply(x, 2, function(y) round(y *(max(x)/max(y)), 0))},
        scaled = {FUN <- function(x) {
                o <- apply(x, 2, function(y) scale(y, FALSE))
                rownames(o) <- rownames(x)
                o
            }} ,
        stop("`type` must be one of c(\"prop\", \"max\", \"scaled\")")
    )

    out <- FUN(x)
    class(out) <- c("weighted_wfm", class(out))
    attributes(out)[["weighting"]] <- type

    out
}

#' Plots a weighted_wfm object
#' 
#' Plots a weighted_wfm object.
#' 
#' @param x The weighted_wfm object
#' @param non.zero logical.  If \code{TRUE} all values converted to dummy coded 
#' based on x_ij > 0.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param by.column logical.  If \code{TRUE} applies scaling to the column.  If 
#' \code{FALSE}  applies scaling by row (use \code{NULL} to turn off scaling).
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid).  
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param \ldots Other arguments passed to qheat.
#' @method plot weighted_wfm
#' @export
plot.weighted_wfm <- function(x, non.zero = FALSE, digits = 0, by.column = NULL,
    high = ifelse(non.zero, "black", "blue"),  
    grid = ifelse(non.zero, "black", "white"), plot = TRUE, ...) {

    class(x) <- "matrix"

    if (non.zero) {
        if(missing(by.column)) {
            by.column <- NULL
        }
        x <- data.frame(x)
        x[1:ncol(x)] <- lapply(x, function(z) as.numeric(z > 0))
    } else {
        if(missing(by.column)) {
            by.column <- FALSE
        }

    }

    out <- qheat(t(x), digits = digits, high=high, grid = grid,
        by.column = by.column, plot = FALSE, ...) 
 
    if (non.zero) {
        out <- out + guides(fill=FALSE)
    }
    if (plot) {
        print(out)
    }
    invisible(out)
}


#' Filter
#' 
#' \code{Filter} - Filter words from various objects that meet max/min word 
#' length criteria.
#' 
#' @param x A filterable object (e.g., \code{\link[qdap]{wfm}},
#' \code{\link[base]{character}}).
#' @param min Minimum word length.
#' @param max Maximum word length.
#' @param count.apostrophe logical.  If \code{TRUE} apostrophes are counted as 
#' characters.
#' @param stopwords A vector of stop words to remove.
#' @param ignore.case logical.  If \code{TRUE} stopwords will be removed 
#' regardless of case (ignored if used on a \code{\link[qdap]{wfm}}).
#' @param \ldots Other arguments passed to specific Filter methods.
#' @rdname Filter
#' @note The name and idea behind this function is inspired by the \pkg{dplyr}
#' package's \code{filter} function and has a similar meaning in that you are 
#' grabbing rows (or elements) meeting a particular criteria.
#' @export
#' @examples
#' \dontrun{
#' Filter(with(DATA, wfm(state, list(sex, adult))), 5)
#' with(DATA, wfm(state, list(sex, adult)))
#' 
#' ## Filter particular words based on max/min values in wfm
#' v <- with(DATA, wfm(state, list(sex, adult)))
#' Filter(v, 5)
#' Filter(v, 5, count.apostrophe = FALSE)
#' Filter(v, 5, 7)
#' Filter(v, 4, 4)
#' Filter(v, 3, 4)
#' Filter(v, 3, 4, stopwords = Top25Words)
#' 
#' ## Filter works on character strings too...
#' x <- c("Raptors don't like robots!",  "I'd pay $500.00 to rid them.")
#' Filter(x, 3)
#' Filter(x, 4)
#' Filter(x, 4, count.apostrophe = FALSE)
#' Filter(x, 4, count.apostrophe = FALSE, stopwords="raptors")
#' Filter(x, 4, stopwords="raptors")
#' Filter(x, 4, stopwords="raptors", ignore.case = FALSE)
#' 
#' DATA[, "state"] <- Filter(DATA[, "state"], 4)
#' DATA <- qdap::DATA
#' 
#' ## Filter `all_words`
#' head(all_words(raj$dialogue))
#' Filter(head(all_words(raj$dialogue)), min = 3)
#' }
Filter <-
function(x, min = 1, max = Inf, count.apostrophe = TRUE, stopwords = NULL, 
    ignore.case = TRUE, ...){
    
    min
    max
    count.apostrophe
    stopwords
    ignore.case
    UseMethod("Filter")
}

#' Word Frequency Matrix
#' 
#' \code{Filter.wfm} - Filter words from a wfm that meet max/min word length 
#' criteria.
#' 
#' @rdname Filter
#' @export
#' @method Filter wfm
#' @return \code{Filter} - Returns a matrix of the class "wfm".
Filter.wfm <- 
function(x, min = 1, max = Inf, count.apostrophe = TRUE, stopwords = NULL, 
    ...) {

    if (!is.null(stopwords)) {
        x <- x[!rownames(x) %in% stopwords, ]
    }

    nms <- rownames(x)
    if (!count.apostrophe) {
        nms <- gsub("'", "", nms)
    }

    lens <- nchar(nms)
    as.wfm(x[lens >= min & lens <= max, ])

}


#' @export
Filter.default <- function(x, ...) base::Filter
#function(..., min = 1, max = Inf, count.apostrophe, stopwords = NULL, x){
#        LIS <- list(...)
#        return(Filter.wfm(LIS, min, max, count.apostrophe))
#}


is.Integer <- 
function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' Filter
#' 
#' \code{Filter.character} - Filter words from a character vector that meet 
#' max/min word length criteria.
#' 
#' character Method for Filter
#' @rdname Filter
#' @export
#' @method Filter character
#' @return \code{Filter.character} - Returns a vector of the class "character".
#' @return \code{Filter.wfm} - Returns a matrix of the class "wfm".
Filter.character <- function(x, min = 1, max = Inf, count.apostrophe = TRUE, 
    stopwords = NULL, ignore.case = TRUE, ...) {

    splits <- "(\\s+)|%s(?=[[:punct:]])"
    splits <- sprintf(splits, ifelse(count.apostrophe, "(?!')", ""))
    x2 <- lapply(strsplit(x, splits, perl = TRUE), function(y) {
        unblanker(unlist(y))
    })

    if (!is.null(stopwords)) {
        if (ignore.case) {
            stopwords <- c(stopwords, sapply(stopwords, Caps))
        }
        x2 <- lapply(x2, function(x) x[!x %in% stopwords])
    }

    mapply(function(a, b) {paste(a[b >= min & b <= max], 
        collapse = " ")}, x2, lapply(x2, nchar))

}


#' Word Frequency Matrix
#' 
#' \code{as.wfm} - Attempts to coerce a matrix to a \code{\link[qdap]{wfm}}.
#' 
#' @param x An object with words for row names and integer values.
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{as.wfm} - Returns a matrix of the class "wfm".
as.wfm <- function(x, ...){
    
    x

    UseMethod("as.wfm")
}  

#' \code{as.wfm.matrix} - \code{matrix} method for \code{as.wfm} used to 
#' convert matrices to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm matrix
as.wfm.matrix <- function(x, ...) {

    if(!all(is.Integer(x))){
        stop("x must contain only integer values")
    }

    class(x) <- c("wfm", "true.matrix", class(x))
    x    
}


#' \code{as.wfm.default} - Default method for \code{as.wfm} used to 
#' convert matrices to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm default 
as.wfm.default <- function(x, ...) {

    if(!all(is.Integer(x))){
        stop("x must contain only integer values")
    }
    warning("Not a matrix.object; may not convert correctly", immediate. = TRUE)
    x <- as.matrix(x)
    
    class(x) <- c("wfm", "true.matrix", class(x))
    x    
}

#' \code{as.wfm.TermDocumentMatrix} - \code{TermDocumentMatrix} method for 
#' \code{as.wfm} used to a \code{TermDocumentMatrix} to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm TermDocumentMatrix 
as.wfm.TermDocumentMatrix <- function(x, ...) {
  
    tm2qdap(x)
 
}


#' \code{as.wfm.DocumentTermMatrix} - \code{DocumentTermMatrix} method for 
#' \code{as.wfm} used to a \code{DocumentTermMatrix} to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm DocumentTermMatrix 
as.wfm.DocumentTermMatrix <- function(x, ...) {
  
    tm2qdap(x)
 
}

#' \code{as.wfm.data.frame} - data.frame method for \code{as.wfm} used to 
#' convert matrices to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm data.frame 
as.wfm.data.frame <- function(x, ...) {

    if(!all(is.Integer(x))){
        stop("x must contain only integer values")
    }

    x <- as.matrix(x)
    
    class(x) <- c("wfm", "true.matrix", class(x))
    x    
}

#' \code{as.wfm.wfdf} - wfdf method for \code{as.wfm} used to 
#' convert matrices to a \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm wfdf 
as.wfm.wfdf <- function(x, ...) {

    wfm(x)

}


#' \code{as.wfm.Corpus} - Corpus method for \code{as.wfm} used to 
#' convert matrices to a \code{wfm}.
#' @param col The column name (generally not used).
#' @param row The row name (generally not used).
#' @rdname Word_Frequency_Matrix
#' @export
#' @method as.wfm Corpus
as.wfm.Corpus <- function(x, col = "docs", row = "text", ...) {

      text <- docs <- NULL
      with(as.data.frame(x, col1 = col, col2 = row, sent.split = FALSE), 
          wfm(text, docs, ...))  

}

#' \code{wfm.Corpus} - Corpus method for \code{wfm}.
#' @rdname Word_Frequency_Matrix
#' @export
#' @method wfm Corpus 
wfm.Corpus <- function(text.var, ...){
    as.wfm.Corpus(x=text.var)
}

## tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
## Matrix or Document Term Matrix
## 
## \code{tm2qdap} - Convert the \pkg{tm} package's 
## \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}} to
## \code{\link[qdap]{wfm}}.
## 
## @param x A \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
## @return \code{tm2qdap} - Returns a \code{\link[qdap]{wfm}} object or 
## \code{weight} object.
## @rdname
## INTERNAL HELPER FUNCTION TO CONVERT "DocumentTermMatrix", "TermDocumentMatrix"
## TO "wfm"
tm2qdap <- function(x) {

    opts <- c("DocumentTermMatrix", "TermDocumentMatrix")
    cls <- opts[opts %in% class(x)]

    if (cls == "DocumentTermMatrix") {
        x <- t(x)
    }
    
    y <- as.matrix(data.frame(as.matrix(x), check.names = FALSE))
    
    if(!any(attributes(x)[["weighting"]] %in% "tf")){
        class(y) <- c("weighted_wfm", class(y))
    } else {
        class(y) <- c("wfm", "true.matrix", class(y))
    }

    y

}


