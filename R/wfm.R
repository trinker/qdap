#' Word Frequency Matrix
#' 
#' \code{wfm} - Generate a word frequency matrix by grouping variable(s).
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param output Output type (either \code{"proportion"} or \code{"percent"}).
#' @param stopwords A vector of stop words to remove.
#' @param char2space A vector of characters to be turned into spaces.  If 
#' \code{char.keep} is \code{NULL}, \code{char2space} will activate this 
#' argument.
#' @param \ldots Other arguments supplied to \code{\link[qdap]{strip}}.
#' @param digits An integer indicating the number of decimal places (round) or 
#' significant digits (signif) to be used. Negative values are allowed.
#' @param margins logical. If \code{TRUE} provides grouping.var and word 
#' variable totals.
#' @param word.lists A list of character vectors of words to pass to 
#' \code{wf.combine}
#' @param matrix logical.  If \code{TRUE} returns the output as a 
#' \code{\link[qdap]{wfm}} rather than a \code{\link[qdap]{wfdf}} object.
#' @return \code{wfm} - returns a word frequency of the class matrix.
#' @rdname Word_Frequency_Matrix
#' @note Words can be kept as one by inserting a double tilde (\code{"~~"}), or 
#' other character strings passed to char2space, as a single word/entry. This is 
#' useful for keeping proper names as a single unit.
#' @keywords word-frequency-matrix
#' @export
#' @examples
#' \dontrun{
#' ## word frequency matrix (wfm) example:
#' with(DATA, wfm(state, list(sex, adult)))[1:15, ]
#' with(DATA, wfm(state, person))[1:15, ]
#' 
#' ## insert double tilde ("~~") to keep phrases(i.e., first last name)
#' alts <- c(" fun", "I ")
#' state2 <- mgsub(alts, gsub("\\s", "~~", alts), DATA$state) 
#' with(DATA, wfm(state2, list(sex, adult)))[1:18, ]
#' 
#' ## word frequency dataframe (wfdf) example:
#' with(DATA, wfdf(state, list(sex, adult)))[1:15, ]
#' with(DATA, wfdf(state, person))[1:15, ]
#' 
#' ## insert double tilde ("~~") to keep dual words (i.e., first last name)
#' alts <- c(" fun", "I ")
#' state2 <- mgsub(alts, gsub("\\s", "~~", alts), DATA$state)
#' with(DATA, wfdf(state2, list(sex, adult)))[1:18, ]
#' 
#' ## wfm.expanded example:
#' z <- wfm(DATA$state, DATA$person)
#' wfm.expanded(z)[30:45, ] #two "you"s
#' 
#' ## wf.combine examples:
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
#' wf.combine(z, WL2) #Won't work not a raw frequency matrix     
#' wf.combine(x, WL2) #Works (raw and no margins)                     
#' wf.combine(y, WL2) #Works (raw with margins)                           
#' wf.combine(y, c("you", "your", "your're"))                        
#' wf.combine(y, WL1)                                                  
#' wf.combine(y, WL3)                                                   
#' ## wf.combine(y, WL4) #Error         
#' wf.combine(y, WL5)                                         
#' wf.combine(y, WL6)                                              
#' wf.combine(y, WL7)                                           
#'                                                                   
#' worlis <- c("you", "it", "it's", "no", "not", "we")              
#' y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)  
#' z <- wf.combine(y, worlis, matrix = TRUE)                      
#'                                                                  
#' chisq.test(z)                                                      
#' chisq.test(wfm(wfdf = y)) 
#' }
wfm <-
function(text.var = NULL, grouping.var = NULL, 
         output = "raw", stopwords = NULL, char2space = "~~", ...){

    if (is(text.var, "wfdf")) {
        if (is(text.var, "t.df")) {
            wfdf <- text.var
        } else {
            if (is(text.var, "m.df")) { 
                wfdf <- text.var[-nrow(text.var), -ncol(text.var)]
            } else {
                stop("Object must be a raw word frequency data frame")
            }
        }
        x2 <- wfdf[, -1, drop = FALSE]
        rownames(x2) <- wfdf[, 1]
        x2 <- as.matrix(x2)
    } else {
        if(is.null(grouping.var)){
            grouping <- rep("all", length(text.var))
        } else {
            if (is.list(grouping.var) & length(grouping.var)>1) {
                grouping <- paste2(grouping.var)
            } else {
                grouping <- unlist(grouping.var)
            } 
        } 
        txt <- strip(text.var, char.keep = char2space, 
            apostrophe.remove = FALSE, ...)
        txtL <- lapply(split(txt, grouping), function(x) {
              table(unlist(strsplit(x, "\\s+")))
        })
        rnms <- sort(unique(unlist(lapply(txtL, names))))
        txtL <- lapply(txtL, data.frame)
        txtL <- lapply(txtL, function(x) {
            new <- rnms[!rnms %in% x[, "Var1"]]
            DF <- rbind.data.frame(x, data.frame(Var1 = new, 
                Freq = rep(0, length(new))))
            DF[order(as.character(DF$Var1)), 2]
        })
        x2 <- do.call(cbind, txtL)
        if (!is.null(char2space)) {
            rownames(x2) <- mgsub(char2space, " ", rnms)
        } else {
            rownames(x2) <- rnms
        }
        if (!is.null(stopwords)){
            x2 <- x2[!rownames(x2) %in% stopwords, ]
        }
        if (output != "raw"){
            x2 <- x2/colSums(x2)
            if (output == "percent") {
                x2 <- x2*100
            }
            class(x2) <- c("wfm", "prop.matrix", class(x2))
            return(x2)
        }
    }
    class(x2) <- c("wfm", "true.matrix", class(x2))
    x2
}

#' Prints an wfm Object
#' 
#' Prints an wfm object.
#' 
#' @param x The wfm object.
#' @param \ldots ignored
#' @method print wfm
#' @S3method print wfm
print.wfm <-
  function(x, digits = 3, ...) {
    class(x) <- "matrix"
    print(round(x, digits = digits))
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
    x <- lapply(bl, bag.o.words, char.keep = char2space, ...)
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
#' \code{wfm.expanded} - Expand a word frequency matrix to have multiple rows 
#' for each word.
#' 
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wfm.expanded} - returns a matrix similar to a word frequency 
#' matrix (\code{wfm}) but the rows are expanded to represent the maximum usages 
#' of the word and cells are dummy coded to indicate that number of uses.
wfm.expanded <-
function(text.var, grouping.var = NULL, ...){
    if(is(text.var, "true.matrix")) {
        z <- text.var
    } else {
        if(is(text.var, "m.df")){
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


#' Combined Word Frequency Data Frame
#' 
#' \code{wf.combine} - Combines words (rows) of a word frequency dataframe 
#' (\code{wfdf}) together.
#'
#' @param wf.obj A \code{wfm} or \code{wfdf} object.
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wf.combine} - returns a word frequency matrix (\code{wfm}) or 
#' dataframe (\code{wfdf}) with counts for the combined word.lists merged and 
#' remaining terms (\code{else}).
wf.combine <-
function(wf.obj, word.lists, matrix = FALSE){
    suppressWarnings(if (is.list(word.lists) & length(word.lists) > 1 & 
        any(Reduce("%in%", word.lists))) {
        stop("overlapping words in word.lists")
    })
    if (is(wf.obj, "t.df")) {
        wf.obj <- wf.obj
    } else {
   
        if (is(wf.obj, "m.df")) { 
            wf.obj <- wf.obj [-nrow(wf.obj), -ncol(wf.obj)]
        } else {
            if (!is(wf.obj, "true.matrix")) {
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
    if(!is.list(word.lists)) word.lists <- list(word.lists)
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
    DFF <- data.frame(word.group = NAMES, m)
    if (matrix) {
        DFF2 <- as.matrix(DFF[, -1])
        rownames(DFF2) <- as.character(DFF[, 1])
        DFF <- DFF2
        class(DFF) <- c("wfm", "true.matrix", class(DFF))
        return(DFF)
    }
    class(DFF) <- c("wfdf", class(DFF))
    DFF
}