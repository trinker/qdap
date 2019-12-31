#' Word Counts
#' 
#' \code{word_count} - Transcript apply word counts.
#' 
#' @rdname word_count
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param byrow logical.  If \code{TRUE} counts by row, if \code{FALSE} counts 
#' all words.
#' @param missing Value to insert for missing values (empty cells).
#' @param digit.remove logical.  If \code{TRUE} removes digits before counting 
#' words.
#' @param names logical.  If \code{TRUE} the sentences are given as the names of 
#' the counts.
#' @param apostrophe.remove logical.  If \code{TRUE} apostrophes will be counted 
#' in the character count.
#' @param count.space logical.  If \code{TRUE} spaces are counted as characters.
#' @param prop.by.row logical.  If \code{TRUE} applies proportional to the row.  
#' If \code{FALSE} applies by column.
#' @param \ldots Other arguments passed to \code{\link[qdap]{prop}}.
#' @return \code{word_count} - returns a word count by row or total.
#' @note wc is a convenient short hand for word_count.
#' @seealso \code{\link[qdap]{syllable_count}},
#' \code{\link[qdap]{prop}},
#' \code{\link[qdap]{colcomb2class}}
#' @export 
#' @examples
#' \dontrun{
#' ## WORD COUNT
#' word_count(DATA$state)
#' wc(DATA$state)
#' word_count(DATA$state, names = TRUE)
#' word_count(DATA$state, byrow=FALSE, names = TRUE)
#' sum(word_count(DATA$state))
#' 
#' sapply(split(raj$dialogue, raj$person), wc, FALSE) %>%
#'     sort(decreasing=TRUE) %>% 
#'     list2df("wordcount", "person") %>%
#'     `[`(, 2:1)
#' 
#' ## PLOT WORD COUNTS
#' raj2 <- raj
#' raj2$scaled <- unlist(tapply(wc(raj$dialogue), raj2$act, scale))
#' raj2$scaled2 <- unlist(tapply(wc(raj$dialogue), raj2$act, scale, scale = FALSE))
#' raj2$ID <- factor(unlist(tapply(raj2$act, raj2$act, seq_along)))
#' 
#' ggplot(raj2, aes(x = ID, y = scaled, fill =person)) +
#'     geom_bar(stat="identity") +
#'     facet_grid(act~.) + 
#'     ylab("Scaled") + xlab("Turn of Talk") +
#'     guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
#'     theme(legend.position="bottom") +
#'     ggtitle("Scaled and Centered")
#' 
#' 
#' ggplot(raj2, aes(x = ID, y = scaled2, fill =person)) +
#'     geom_bar(stat="identity") +
#'     facet_grid(act~.) + 
#'     ylab("Scaled") + xlab("Turn of Talk") +
#'     guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
#'     theme(legend.position="bottom") +
#'     ggtitle("Mean Difference")
#'   
#'     
#' raj$wc <- wc(raj$dialogue)
#' raj$cum.wc <- unlist(with(raj, tapply(wc, act, cumsum)))
#' raj$turn <- unlist(with(raj, tapply(act, act, seq_along)))
#' ggplot(raj, aes(y=cum.wc, x=turn)) + 
#'     geom_step(direction = "hv") + 
#'     facet_wrap(~act)
#'         
#' ## CHARACTER COUNTS
#' character_count(DATA$state)
#' character_count(DATA$state, byrow=FALSE)
#' sum(character_count(DATA$state))
#' 
#' ## CHARACTER TABLE
#' x <- character_table(DATA$state, DATA$person)
#' plot(x)
#' plot(x, label = TRUE)
#' plot(x, label = TRUE, text.color = "red")
#' plot(x, label = TRUE, lab.digits = 1, zero.replace = "PP7")
#' 
#' scores(x)
#' counts(x)
#' proportions(x)
#' 
#' plot(scores(x))
#' plot(counts(x))
#' plot(proportions(x))
#' 
#' ## combine columns
#' colcomb2class(x, list(vowels = c("a", "e", "i", "o", "u")))
#' 
#' ## char_table(DATA$state, DATA$person)
#' ## char_table(DATA$state, DATA$person, percent = TRUE)
#' ## character_table(DATA$state, list(DATA$sex, DATA$adult))
#' 
#' library(ggplot2);library(reshape2)
#' dat <- character_table(DATA$state, list(DATA$sex, DATA$adult))
#' dat2 <- colsplit2df(melt(counts(dat)), keep.orig = TRUE)
#' head(dat2, 15)
#' 
#' ggplot(data = dat2, aes(y = variable, x = value, colour=sex)) +
#'     facet_grid(adult~.) +
#'     geom_line(size=1, aes(group =variable), colour = "black") +
#'     geom_point()
#' 
#' ggplot(data = dat2, aes(x = variable, y = value)) +
#'     geom_bar(aes(fill = variable), stat = "identity") +
#'     facet_grid(sex ~ adult, margins = TRUE) +
#'     theme(legend.position="none")
#' }
word_count <- 
function(text.var, byrow = TRUE, missing = NA, digit.remove = TRUE, 
    names = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | len == 1 && (is.na(x) | is.null(x)), missing, len)
    }
    txt <- rm_stopwords(text.var, strip = TRUE,  digit.remove = digit.remove, 
        stopwords = NULL)
    z <- sapply(txt, len2, missing = missing)
    if (!byrow) {
        return(sum(z, na.rm = TRUE)   )
    }
    if(names) {
        names(z) <- text.var
    }
    z
}

#' @rdname word_count
#' @export
wc <- word_count

#' Count Number of Characters
#' 
#' \code{character_count} - Transcript apply character counts.
#' 
#' @return \code{character_count} - returns a character count by row or total.
#' @rdname word_count
#' @export
character_count <- 
function(text.var, byrow = TRUE, missing = NA, apostrophe.remove = TRUE,
    digit.remove = TRUE, count.space = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | (is.na(x) | is.null(x)), missing, nchar(x))
    }
    txt <- rm_stopwords(text.var, strip = TRUE,  separate =  FALSE,
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
#' \code{character_table} - Computes a table of character counts by grouping .
#' variable(s).
#' 
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param zero.replace Value to replace 0 values with.
#' @param digits Integer; number of decimal places to round when printing.   
#' @return \code{character_table} - returns a list:
#' dataframe of character counts by grouping variable.
#' \item{raw}{Dataframe of the frequency of characters by grouping variable.} 
#' \item{prop}{Dataframe of the proportion of characters by grouping variable.} 
#' \item{rnp}{Dataframe of the frequency and proportions of characters by 
#' grouping variable.} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @rdname word_count
#' @export
character_table <- function(text.var, grouping.var=NULL, percent = TRUE, 
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
    L1 <- lapply(split(DF$text.var, DF$grouping), stats::na.omit)
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
    
    out <- list(raw = DF2, prop = DF3, rnp = rnp)  
    attributes(out) <- list(
            class = "character_table",
            names = names(out),
            percent = percent, 
            zero.replace = zero.replace,
            digits = digits
    )
    out
}

#' Prints a character_table object
#' 
#' Prints a character_table object.
#' 
#' @param x The character_table object
#' @param digits Integer values specifying the number of digits to be 
#' printed.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.  If \code{NULL} uses the value from 
#' \code{\link[qdap]{termco}}.  Only used if \code{label} is \code{TRUE}.
#' @param zero.replace Value to replace 0 values with.  If \code{NULL} uses the 
#' value from \code{\link[qdap]{termco}}.  Only used if \code{label} is 
#' \code{TRUE}.
#' @param \ldots ignored
#' @method print character_table
#' @export
print.character_table <-
function(x, digits = 2, percent = NULL, zero.replace = NULL, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    if (!is.null(percent)) {
        if (percent != attributes(x)[["percent"]]) {
            DF <- as.matrix(x$prop[, -c(1:2)])
            if (percent) {
                DF <- DF*100    
            } else {
                DF <-  DF/100
            }
            x$prop <- data.frame(x$prop[, 1:2], DF, check.names = FALSE) 
        }
    } else {
        percent <- attributes(x)[["percent"]]
    }
    if (is.null(zero.replace)) {
        zero.replace <- attributes(x)[["zero.replace"]]
    }
    if (is.null(digits)) {
        digits <- attributes(x)[["digits"]]
    }
    rnp <- raw_pro_comb(x$raw[, -1, drop = FALSE], 
        x$prop[, -1, drop = FALSE], digits = digits, percent = percent, 
        zero.replace = zero.replace, override = TRUE)  
    rnp <- data.frame(x$raw[, 1, drop = FALSE], rnp, check.names = FALSE)     
    print(rnp)
    options(width=WD)
}

#' Plots a character_table Object
#' 
#' Plots a character_table  object.
#' 
#' @param x The character_table  object
#' @param label logical.  If \code{TRUE} the cells of the heat map plot will be 
#' labeled with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is \code{TRUE}.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.  If \code{NULL} uses the value from 
#' \code{\link[qdap]{question_type}}.  Only used if \code{label} is \code{TRUE}.
#' @param zero.replace Value to replace 0 values with.  If \code{NULL} uses the 
#' value from \code{\link[qdap]{question_type}}.  Only used if \code{label} is 
#' \code{TRUE}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}
#' @method plot character_table 
#' @export
plot.character_table <- function(x, label = FALSE, lab.digits = 1, percent = NULL, 
    zero.replace = NULL, ...) {
    
    if (label) {
        if (!is.null(percent)) {
            if (percent != attributes(x)[["percent"]]) {
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
            percent <- attributes(x)[["percent"]] 
        }
        if (is.null(zero.replace)) {
            zero.replace <- attributes(x)[["zero.replace"]]
        }
        if (is.null(lab.digits)) {
            lab.digits <- attributes(x)[["digits"]]
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


#' @rdname word_count
#' @export
char_table <- character_table

 #' Term Counts
#' 
#' View character_table scores.
#' 
#' character_table Method for scores
#' @param x The \code{\link[qdap]{character_table}} object.
#' @param \ldots ignored
#' @export
#' @method scores character_table
scores.character_table <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "character_table_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Term Counts
#' 
#' View character_table counts.
#' 
#' character_table Method for counts
#' @param x The \code{\link[qdap]{character_table}} object.
#' @param \ldots ignored
#' @export
#' @method counts character_table
counts.character_table <- function(x, ...) {

    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "character_table_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Term Counts
#' 
#' View \code{\link[qdap]{character_table}} proportions.
#' 
#' character_table Method for proportions
#' @param x The character_table object.
#' @param \ldots ignored
#' @export
#' @method proportions character_table
proportions.character_table <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "character_table_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

