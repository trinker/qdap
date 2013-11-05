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
#' @param \ldots Other arguments supplied to \code{\link[qdap]{strip}}.
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
#' @examples
#' \dontrun{
#' ## word frequency matrix (wfm) example:
#' with(DATA, wfm(state, list(sex, adult)))[1:15, ]
#' with(DATA, wfm(state, person))[1:15, ]
#' with(DATA, wfm(state, list(sex, adult)))
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
#' ## insert double tilde ("~~") to keep phrases (e.g., first last name)
#' alts <- c(" fun", "I ")
#' state2 <- mgsub(alts, gsub("\\s", "~~", alts), DATA$state)
#' with(DATA, wfdf(state2, list(sex, adult)))[1:18, ]
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
#' wfm_combine(z, WL2) #Won't work not a raw frequency matrix     
#' wfm_combine(x, WL2) #Works (raw and no margins)                     
#' wfm_combine(y, WL2) #Works (raw with margins)                           
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
#' ## Words correlated within turns of talk
#' library(reports)
#' x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
#' dat <- wfm(rajSPLIT$dialogue, x)
#' 
#' 
#' cor(t(dat)[, c("romeo", "juliet")])
#' cor(t(dat)[, c("romeo", "banished")])
#' cor(t(dat)[, c("romeo", "juliet", "hate", "love")])
#' qheat(cor(t(dat)[, c("romeo", "juliet", "hate", "love")]), 
#'     diag.na = TRUE, values = TRUE, digits = 3, by.column = NULL)
#'     
#' dat2 <- wfm(DATA$state, seq_len(nrow(DATA)))
#' qheat(cor(t(dat2)), low = "yellow", high = "red", 
#'     grid = "grey90", diag.na = TRUE, by.column = NULL)
#'     
#' ## With `word_cor`
#' worlis <- list(
#'     pronouns = c("you", "it", "it's", "we", "i'm", "i"),
#'     negative = qcv(no, dumb, distrust, not, stinks),
#'     literacy = qcv(computer, talking, telling)
#' )
#' y <- wfdf(DATA$state, id(DATA, prefix = TRUE))
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
#' fit <- ca(mytable)
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
#' wfm_weight(WFM, "prop")
#' wfm_weight(WFM, "max")
#' wfm_weight(WFM, "scaled")
#' }
wfm <- 
function(text.var = NULL, grouping.var = NULL, output = "raw", stopwords = NULL, 
    char2space = "~~", ...){

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
              unlist(strsplit(x, "\\s+"))
        })

        ## tabulate frequencies per word
        x2 <- t(mtabulate(txtL))

        ## replace spaced characters
        if (!is.null(char2space)) {
            rownames(x2) <- mgsub(char2space, " ", rownames(x2))
        } 

        if (!is.null(stopwords)){
            x2 <- x2[!rownames(x2) %in% tolower(stopwords), , drop = FALSE]
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
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
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
    if(!is.list(word.lists)) {
        word.lists <- list(word.lists)
    }
    if (is(wf.obj, "true.matrix")) {
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
        DFF <- DFF2
        class(DFF) <- c("wfm", "true.matrix", class(DFF))
        return(DFF)
    }
    class(DFF) <- c("wfdf", class(DFF))
    DFF
}


#' Plots a wfm object
#' 
#' Plots a wfm object.
#' 
#' @param x The wfm object
#' @param non.zero logical.  If \code{TRUE} all values coverted to dummy coded 
#' based on x_ij > 0.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param by.column logical.  If \code{TRUE} applies scaling to the column.  If 
#' \code{FALSE}  applies scaling by row (use \code{NULL} to turn off scaling).
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid).  
#' @param \ldots Other arguments passed to qheat.
#' @method plot wfm
#' @S3method plot wfm
plot.wfm <- function(x, non.zero = FALSE, digits = 0, by.column,
    high = ifelse(non.zero, "black", "blue"),  
    grid = ifelse(non.zero, "black", "white"), ...) {

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
        by.column = by.column, ...) 
 
    invisible(out)
}


#' Plots a wfdf object
#' 
#' Plots a wfdf object.
#' 
#' @param x The wfdf object
#' @param \ldots Other arguments passed to \code{\link[qdap]{plot.wfm}}.
#' @method plot wfdf
#' @S3method plot wfdf
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
    out <- paste(
        sprintf("A word-frequency matrix (%s terms, %s groups)", nrow(x), ncol(x)),
        "\n", sprintf("Non-/sparse entries       : %s/%s", Y, N),
        sprintf("Sparsity                  : %s%%", sparsity),
        sprintf("Maximal term length       : %s", max(NCHAR)) ,
        sprintf("Less than four characters : %s%%", 100*round(sum(NCHAR < 4)/nrow(x), 2)) ,
        sprintf("Hapax legomenon           : %s(%s%%)", HL, 100*round(HL/nrow(x), 2)),
        sprintf("Dis legomenon             : %s(%s%%)", DL, 100*round(DL/nrow(x), 2)),
        sprintf("Shannon's diversity index : %s", round(shan, 2)),
    sep="\n")
    message(out)
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
#' \code{wfm_weight} - Weight a word frequency matrix for analysis were such 
#' weighting is sensible..
#' 
#' @param wfm.obj A \code{\link[qdap]{wfm}} object.
#' @param type The type of weighting to use: c(\code{"prop"}, \code{"max"}, 
#' \code{"scaled"}).  All weight by column.  \code{"prop"} uses a proportion
#' weighting and all columns sum to 1.  \code{"max"} weights in proportion to 
#' the max value; all values are integers and column sums may not be equal.
#' \code{"scaled"} uses \code{\link[base]{scale}} to scale with 
#' \code{center = FALSE}; output is not integer and column sums may not be 
#' equal.
#' @rdname Word_Frequency_Matrix
#' @export
#' @return \code{wfm_weight} - Returns a weighted matrix for use with other R 
#' packages. The output is not of the class "wfm".
wfm_weight <- function(wfm.obj, type = "prop") {

    if (is(wfm.obj, "wfdf") && !is(wfm.obj, "f.df")) {
        wfm.obj <- wfm(wfm.obj)
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
                rownames(o) <- rownames(wfm.obj)
                o
            }} ,
        stop("`type` must be one of c(\"prop\", \"max\", \"scaled\")")
    )

    out <- FUN(wfm.obj)
    class(out) <- c("weighted_wfm", class(out))
    attributes(out)[["Weighting"]] <- type

    out
}

