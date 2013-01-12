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
#' @param proportional logical.  If TRUE outputs the table proportionally 
#' (see \code{\link[qdap]{prop}}).
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
#' char.table(DATA$state, DATA$person)
#' char.table(DATA$state, DATA$person, proportianal = TRUE)
#' character.table(DATA$state, list(DATA$sex, DATA$adult))
#' colsplit2df(character.table(DATA$state, list(DATA$sex, DATA$adult)))
#' 
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
#' @return \code{character.table} - returns a dataframe of character counts by 
#' grouping variable.
#' @rdname word.count
#' @export
character.table <- function(text.var, grouping.var, proportional = FALSE, 
    prop.by.row = TRUE, ...) {
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
    if (proportional) {
        DF2 <- data.frame(DF2[, 1, drop = FALSE], prop(DF2[-1], 
            by.column = (1 - prop.by.row), ...), check.names = FALSE)
        DF2[is.nan(DF2)] <- 0
    }
    class(DF2) <- "char_tab"
    DF2
}

#' Prints a char_tab object
#' 
#' Prints a char_tab object.
#' 
#' @param x The char_tab object
#' @param \ldots ignored
#' @method print char_tab
#' @S3method print char_tab
print.char_tab <-
function(x, ...) {
    class(x) <- "data.frame"
    print(x)
}


#' Plots a char_tab object
#' 
#' Plots a char_tab object.
#' 
#' @param x The char_tab object
#' @param \ldots Other arguments passed to qheat
#' @method plot char_tab
#' @S3method plot char_tab
plot.char_tab <- function(x, ...) {
    class(x) <- "data.frame"
    qheat(x, ...)
}


#' @rdname word.count
#' @export
char.table <- character.table