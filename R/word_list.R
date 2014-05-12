#' Raw Word Lists/Frequency Counts
#' 
#' Transcript Apply Raw Word Lists and Frequency Counts by grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param stopwords A vector of stop words to remove.
#' @param alphabetical If \code{TRUE} the output of frequency lists is ordered 
#' alphabetically.  If \code{FALSE} the list is ordered by frequency rank.
#' @param cut.n Cut off point for reduced frequency stop word list (rfswl).
#' @param cap logical. If \code{TRUE} capitalizes words from the cap.list.
#' @param cap.list Vector of words to capitalize.
#' @param cap.I logical. If \code{TRUE} capitalizes words containing the 
#' personal pronoun I.
#' @param rm.bracket logical If \code{TRUE} all brackets and bracketed text are 
#' removed from analysis.
#' @param char.keep A character vector of symbols (i.e., punctuation) that 
#' \code{word_list} should keep.  The default is to remove every symbol except 
#' apostrophes.
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophes from 
#' the output.
#' @param \ldots Other arguments passed to \code{\link[qdap]{strip}}.
#' @return An object of class \code{"word_list"} is a list of lists of vectors 
#' or dataframes containing the following components: 
#' \item{cwl}{complete word list; raw words}
#' \item{swl}{stop word list; same as rwl with stop words removed}
#' \item{fwl}{frequency word list; a data frame of words and corresponding 
#' frequency counts}
#' \item{fswl}{frequency stopword word list; same as fwl but with stop words 
#' removed}
#' \item{rfswl}{reduced frequency stopword word list; same as fswl but truncated 
#' to n rows}
#' @keywords word-list
#' @export
#' @examples
#' \dontrun{
#' word_list(raj.act.1$dialogue)
#' 
#' out1 <- with(raj, word_list(text.var = dialogue, 
#'     grouping.var = list(person, act)))
#' names(out1)
#' lapply(out1$cwl, "[", 1:5)
#' 
#' with(DATA, word_list(state, person))
#' with(DATA, word_list(state, person, stopwords = Top25Words))
#' with(DATA, word_list(state, person, cap = FALSE, cap.list=c("do", "we")))
#' }
word_list <- 
function(text.var, grouping.var = NULL, stopwords = NULL, alphabetical = FALSE,
    cut.n = 20, cap = TRUE, cap.list=NULL, cap.I=TRUE, rm.bracket = TRUE,
    char.keep = NULL, apostrophe.remove = FALSE, ...) {
    upper <- function(x) paste(substring(x, 1, 1), 
        substring(x, 2, nchar(x)), sep="")
    Sw1 <- stopwords[!substring(stopwords, 1, 1) %in% LETTERS]
    Sw2 <- stopwords[substring(stopwords, 1, 1) %in% LETTERS]
    Sw3 <- if (!is.null(Sw2)) {
        paste0(Sw2, "'s")
    } else {
        NULL
    }
    stopwords <- c(Sw1, upper(Sw2), Sw2, tolower(Sw2), Sw3, tolower(Sw3))
    if(is.null(grouping.var)) {
        dat <- as.data.frame(rep("all", length(text.var)), 
            drop = FALSE, stringsAsFactors = FALSE)
        names(dat) <- "all"
        grouping.var <- dat
    }
    group.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
        apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))){
                    NA
                } else {
                    paste(x, collapse = ".")
                }
            }
        )
    } else {
        grouping.var
    }   
    NAME <- if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)], USE.NAMES = FALSE)
            paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        G[length(G)]
    }
    word_lists1 <- textLISTER(text.var = text.var, group.vars = group.var,
        rm.bracket = rm.bracket, char.keep = char.keep, apostrophe.remove = apostrophe.remove, ...)
    words.UNLISTED <- lapply(word_lists1, function(x) {
            y <- unlist(x)
            names(y) <- NULL
            return(y)
        }
    ) 
    if (cap) { 
        word_lists2 <- lapply(word_lists1, function(x) {
                y <- capitalizer(x, caps.list=cap.list)
                names(y) <- NULL
                return(y)
            }
        )    
    } else {
        word_lists2 <- lapply(word_lists1, function(x) {
                y <- unlist(x)
                names(y) <- NULL
                return(y)
            }
        ) 
    }
    naomit <- function(x) x[!is.na(x)]
    word_lists2 <- lapply(word_lists2, naomit)
    stopped.word_list <- lapply(words.UNLISTED, function(x) {
            x[!x %in% stopwords]
        }
    )
    stopped.word_list <- lapply(stopped.word_list, naomit)
    stopped.word_list <- lapply(stopped.word_list, function(x){ 
            capitalizer(x, caps.list = cap.list)
        }
    )
    COUNT <- function(x) {
        if (is.null(x) | identical(x, character(0)) | 
            identical(x, logical(0))) {
            DF <- data.frame(WORD=NA, FREQ=NA)
        } else {
            DF <- data.frame(table(x))
            names(DF) <- c("WORD", "FREQ")
            DF$WORD <- as.character(DF$WORD)
            DF$FREQ <- as.numeric(DF$FREQ)
            DF <- DF[order(-DF$FREQ, DF$WORD), ]
            rownames(DF) <- NULL
        }
        return(DF)
    }
    freq.word_list <- lapply(word_lists2, COUNT)
    freq.stop.word_list <- lapply(stopped.word_list, COUNT)
    red.freq.stop.word_list <- ncutWORDS(freq.stop.word_list, cut.n = cut.n) 
    word_lists2 <- lapply(word_lists2, function(x) {
            if (is.null(x)){
                return(x)
            } else { 
                class(x) <- c("bagOwords", class(x))
                return(x)
            }
        }
    )
    stopped.word_list <- lapply(stopped.word_list, function(x) {
        if (is.null(x)){
                return(x)
        } else { 
                class(x) <- c("bagOwords", class(x))                
                return(x)
            }
        }
    )
    freq.word_list <- lapply(freq.word_list, function(x) {
            if (is.null(x)) {
                return(x)
            } else { 
                class(x) <- c("freqList", class(x))
                return(x)
            }
        }
    )
    freq.stop.word_list <- lapply(freq.stop.word_list, function(x) {
            if (is.null(x)) {
                return(x)
            } else { 
                class(x) <- c("freqList", class(x))
                return(x)
            }
        }
    )
    red.freq.stop.word_list <- lapply(red.freq.stop.word_list, function(x) {
            if (is.null(x)){
                return(x)
            } else { 
                class(x) <- c("freqList", class(x))
                return(x)
            }
        }
    )
    class(word_lists2) <- c("cwl", class(word_lists2))
    class(stopped.word_list) <- c("swl", class(stopped.word_list))
    class(freq.word_list) <- c("fwl" , class(freq.word_list))
    class(freq.stop.word_list) <- c("fswl", class(freq.stop.word_list))
    class(red.freq.stop.word_list) <- c("rfswl", class(red.freq.stop.word_list))
    if (alphabetical) {
        asort <- function(dat, col=1) {
            dat2 <-dat[order(dat[, col]), ]
            rownames(dat2) <- NULL
            return(dat2)
        }
        freq.word_list <- lapply(freq.word_list, asort)
        freq.stop.word_list <- lapply(freq.stop.word_list, asort)
        red.freq.stop.word_list <- lapply(red.freq.stop.word_list, asort)
    } 
    o <- list(cwl = word_lists2, swl = stopped.word_list, 
        fwl = freq.word_list, fswl = freq.stop.word_list, 
        rfswl = red.freq.stop.word_list)
    class(o) <- c("word_list", "data.frame")
    return(o)
}

#' Prints a word_list Object
#' 
#' Prints a word_list object.
#' 
#' @param x The word_list object
#' @param \ldots ignored
#' @method print word_list
#' @export
print.word_list <-
function(x, ...) {
    class(x) <- "list"
    y <- x[["rfswl"]]
    class(y) <- "list"
    print(y)
}


#' Filter
#' 
#' \code{Filter.fwl} - Filter words from a fwl 
#' that meet max/min word length criteria.
#' 
#' fwl Method for Filter
#' @rdname Filter
#' @export
#' @method Filter fwl
#' @return \code{Filter.fwl} - Returns a matrix of the class "fwl".
Filter.fwl <- function(x, min = 1, max = Inf, 
    count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...) {

    lapply(x, word_list_filter_helper, min = min, max = max, 
        count.apostrophe = count.apostrophe, stopwords = stopwords, 
        ignore.case = ignore.case, ...)
}

word_list_filter_helper <- function(x, min, max, freq =1, 
    count.apostrophe, stopwords, ignore.case, ...) {

    if (!is.null(stopwords)) {
        x[, 1] <- strip(x[, 1], apostrophe.remove = !count.apostrophe, 
            lower.case = ignore.case, ...)
        stopwords <- strip(stopwords, apostrophe.remove = !count.apostrophe, 
            lower.case = ignore.case, ...)
    }

    if (!is.null(stopwords)) {
        x <- x[!x[, 1] %in% stopwords, ]
    }

    lens <- nchar(x[, 1])
    x <- x[lens >= min & lens <= max, ]
    x <- x[x[, 2] >= freq, ]
    class(x) <- c("filtered_word_list", class(x))
    x
}


#' Filter
#' 
#' \code{Filter.fswl} - Filter words from a fswl 
#' that meet max/min word length criteria.
#' 
#' fswl Method for Filter
#' @rdname Filter
#' @export
#' @method Filter fswl
#' @return \code{Filter.fswl} - Returns a matrix of the class "fswl".
Filter.fswl <- function(x, min = 1, max = Inf,
    count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...) {

    lapply(x, word_list_filter_helper, min = min, max = max, 
        count.apostrophe = count.apostrophe, stopwords = stopwords, 
        ignore.case = ignore.case, ...)
}


#' Filter
#' 
#' \code{Filter.rfswl} - Filter words from a rfswl 
#' that meet max/min word length criteria.
#' 
#' rfswl Method for Filter
#' @rdname Filter
#' @export
#' @method Filter rfswl
#' @return \code{Filter.rfswl} - Returns a matrix of the class "rfswl".
Filter.rfswl <- function(x, min = 1, max = Inf,  
    count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...) {

    lapply(x, word_list_filter_helper, min = min, max = max, 
        count.apostrophe = count.apostrophe, stopwords = stopwords, 
        ignore.case = ignore.case,  ...)
}
