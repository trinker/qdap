#' Word Counts
#' 
#' \code{word.count} - Transcript apply word counts.
#' 
#' @rdname word.count
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one
#' output for all text.  Also takes a single grouping variable or a list of 1 or
#' more grouping variables.
#' @param byrow logical.  If TRUE counts by row, if FALSE counts all words.
#' @param missing Value to insert for missing values (empty cells).
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @param names logical.  If TRUE the sentences are given as the names of the 
#' counts.
#' @param apostrophe.remove = TRUE logical.  If TRUE apostrophes will be counted 
#' in the character count.
#' @param count.space logical.  If TRUE spaces are counted as characters.
#' @param prop.by.row logical.  If TRUE applies proportional to the row.  If 
#' FALSE applies by column.
#' @param \ldots Other arguments passed to \code{\link[qdap]{prop}}.
#' @return \code{word.count} - returns a word count by row or total.
#' @note wc is a convienent short hand for word.count.
#' @seealso \code{\link[qdap]{syllable.count}}
#' @seealso \code{\link[qdap]{prop}}
#' @keywords word-count, character-count
#' @export 
#' @examples
#' \dontrun{
#' # WORD COUNT
#' word.count(DATA$state)
#' wc(DATA$state)
#' word.count(DATA$state, names = TRUE)
#' word.count(DATA$state, byrow=FALSE, names = TRUE)
#' sum(word.count(DATA$state))
#' 
#' # CHARACTER COUNTS
#' character.count(DATA$state)
#' character.count(DATA$state, byrow=FALSE)
#' sum(character.count(DATA$state))
#' 
#' library(ggplot2)
#' library(reshape2)
#' dat <- character.table(DATA$state, list(DATA$sex, DATA$adult))
#' (dat2 <- colsplit2df(melt(dat), keep.orig = TRUE))
#' head(dat2)
#' dat3 <- dat2[rep(seq_len(dim(dat2)[1]), dat2[, 5]), -5]
#' 
#' ggplot(data = dat2, aes(y = variable, x = value, colour=sex)) +
#'     facet_grid(adult~.) +
#'     geom_line(size=1, aes(group =variable), colour = "black") +
#'     geom_point() 
#'     
#' ggplot(data = dat3, aes(x = variable, fill = variable)) +
#'     geom_bar() + 
#'     facet_grid(sex ~ adult, margins = TRUE) + 
#'     theme(legend.position="none")
#' 
#' # CHARACTER TABLE
#' (x <- character.table(DATA$state, DATA$person))
#' plot(x)
#' plot(x, label = TRUE)
#' plot(x, label = TRUE, text.color = "red")
#' plot(x, label = TRUE, lab.digits = 1, zero.replace = "PP7")
#' x$raw
#' x$prop
#' x$rnp
#' 
#' char.table(DATA$state, DATA$person)
#' char.table(DATA$state, DATA$person, proportianal = TRUE)
#' character.table(DATA$state, list(DATA$sex, DATA$adult))
#' }
word.count <- 
function(text.var, byrow = TRUE, missing = NA, digit.remove = TRUE, 
    names = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | len == 1 && (is.na(x) | is.null(x)), missing, len)
    }
    txt <- stopwords(text.var, strip = TRUE,  digit.remove = digit.remove, 
        stopwords = NULL)
    z <- sapply(txt, len2, missing = missing)
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    if(names) {
        names(z) <- text.var
    }
    z
}

#' @rdname word.count
#' @export
wc <- word.count

#' Count Number of Characters
#' 
#' \code{character.count} - Transcript apply character counts.
#' 
#' @return \code{character.count} - returns a character count by row or total.
#' @rdname word.count
#' @export
character.count <- 
function(text.var, byrow = TRUE, missing = NA, apostrophe.remove = TRUE,
    digit.remove = TRUE, count.space = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | (is.na(x) | is.null(x)), missing, nchar(x))
    }
    txt <- stopwords(text.var, strip = TRUE,  separate =  FALSE,
        digit.remove = digit.remove, stopwords = NULL)
    txt[txt %in% c("", "NA")] <- NA
    if (!count.space) {
        txt <- gsub("\\s+", "", txt)
    }
    z <- unlist(lapply(txt, len2, missing = missing))
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    z
}

#' Table of Character Counts
#' 
#' \code{character.table} - Computes a table of character counts by grouping .
#' variable(s).
#' 
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proption.
#' @param zero.replace Value to replace 0 values with.
#' @param digits Integer; number of decimal places to round when printing.   
#' @return \code{character.table} - returns a list:
#' dataframe of character counts by grouping variable.
#' \item{raw}{Dataframe of the frequency of characters by grouping variable.} 
#' \item{prop}{Dataframe of the proportion of characters by grouping variable.} 
#' \item{rnp}{Dataframe of the frequency and proportions of characters by 
#' grouping variable.} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @rdname word.count
#' @export
character.table <- function(text.var, grouping.var, percent = TRUE, 
    prop.by.row = TRUE, zero.replace = 0, digits = 2, ...) {
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
    ctab <- function(x) {
        table(unlist(strsplit(tolower(scrubber(paste2(x))), NULL)))
    }
    text.var <- as.character(text.var)
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF$grouping <- factor(DF$grouping)
    L1 <- split(DF$text.var, DF$grouping)
    L2 <- lapply(L1, ctab)
    chars <- sort(unique(unlist(lapply(L2, names))))
    L3 <- do.call(rbind, lapply(L2, function(x){
       nots <- chars[!chars %in% names(x)]
       new <- rev(c(x, rep(0, length(nots))))
       if (!identical(nots, character(0))) {
           names(new)[1:length(nots)] <- nots
       }
       new[order(names(new))]
    }))
    DF2 <- data.frame(x = rownames(L3), L3, check.names=FALSE, 
        row.names = NULL)
    colnames(DF2)[1] <- G
    DF3 <- prop(DF2[-1], percent = percent, by.column = (1 - prop.by.row), ...)
    DF3[is.nan(DF3)] <- 0
    DF3 <- data.frame(DF2[, 1, drop = FALSE], DF3, check.names = FALSE)
    rnp <- raw_pro_comb(DF2[, -1], DF3[, -1], digits = digits, 
        percent = percent, zero.replace = zero.replace)  
    rnp <- data.frame(DF2[, 1, drop = FALSE], rnp, check.names = FALSE)  
    o <- list(raw = DF2, prop = DF3, rnp = rnp, percent = percent, 
        zero.replace = zero.replace)  
    class(o) <- "character.table"
    o
}

#' Prints a character.table object
#' 
#' Prints a character.table object.
#' 
#' @param x The character.table object
#' @param \ldots ignored
#' @method print character.table
#' @S3method print character.table 
print.character.table <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    print(x$rnp)
    options(width=WD)
}


#' Plots a character.table Object
#' 
#' Plots a character.table  object.
#' 
#' @param x The character.table  object
#' @param label logical.  If TRUE the cells of the heat map plot will be labeled 
#' with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is TRUE.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proption.  If NULL uses the value from 
#' \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param \ldots Other arguments passed to qheat
#' @method plot character.table 
#' @S3method plot character.table 
plot.character.table <- function(x, label = FALSE, lab.digits = 1, percent = NULL, 
    zero.replace = NULL, ...) {
    if (label) {
        if (!is.null(percent)) {
            if (percent != x$percent) {
                DF <- as.matrix(x$prop[, -1])
                if (percent) {
                    DF <- DF*100    
                } else {
                    DF <-  DF/100
                }
                x$prop <- data.frame(x$prop[, 1, drop = FALSE], DF, 
                    check.names = FALSE) 
            }
        } else {
            percent <- x$percent 
        }
        if (is.null(zero.replace)) {
            zero.replace <- x$zero.replace
        }
        rnp <- raw_pro_comb(x$raw[, -1], x$prop[, -1], 
            digits = lab.digits, percent = percent, 
                zero.replace = zero.replace)  
        rnp <- data.frame(x$raw[, 1, drop = FALSE], rnp, check.names = FALSE) 
        qheat(x$prop, values=TRUE, mat2 = rnp, ...)
    } else {
        qheat(x$prop, ...)  
    }
}

#' @rdname word.count
#' @export
char.table <- character.table