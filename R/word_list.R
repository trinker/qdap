#' Raw Word Lists/Frequency Counts
#' 
#' Transcript Apply Raw Word Lists and Frequency Counts by grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param stopwords A vector of stop words to remove.
#' @param cut.n Cut off point for reduced frequency stop word list (rfswl).
#' @param cap logical. If TRUE capitalizes words from the cap.list.
#' @param cap.list Vector of words to capitalize.
#' @param cap.I logical. If TRUE capitalizes words containing the personal 
#' pronoun I.
#' @return An object of class \code{"word.list"} is a list containing at the 
#' following components: 
#' \item{cwl}{complete word list; raw words}
#' \item{swl}{stop word list; same as rwl with stop words removed}
#' \item{fwl}{frequency word list; a data frame of words and correspnding 
#' frequency counts}
#' \item{fswl}{fequency stopword word list; same as fwl but with stopwords 
#' removed}
#' \item{rfswl}{reduced frequency stopword word list; same as fswl but truncated 
#' to n rows}
#' @keywords word-list
#' @export
#' @examples
#' \dontrun{
#' XX <-word_list(raj.act.1$dialogue)
#' names(XX)
#' XX$cwl
#' XX$swl
#' XX$fwl 
#' XX$fswl 
#' XX$rfswl 
#' 
#' with(raj, word_list(text.var = dialogue, grouping.var = list(person, act)))
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
    word.lists1 <- textLISTER(text.var = text.var, group.vars = group.var,
        rm.bracket = rm.bracket, char.keep = char.keep, apostrophe.remove = apostrophe.remove, ...)
    words.UNLISTED <- lapply(word.lists1, function(x) {
            y <- unlist(x)
            names(y) <- NULL
            return(y)
        }
    ) 
    if (cap) { 
        word.lists2 <- lapply(word.lists1, function(x) {
                y <- capitalizer(x, caps.list=cap.list)
                names(y) <- NULL
                return(y)
            }
        )    
    } else {
        word.lists2 <- lapply(word.lists1, function(x) {
                y <- unlist(x)
                names(y) <- NULL
                return(y)
            }
        ) 
    }
    naomit <- function(x) x[!is.na(x)]
    word.lists2 <- lapply(word.lists2, naomit)
    stopped.word.list <- lapply(words.UNLISTED, function(x) {
            x[!x %in% stopwords]
        }
    )
    stopped.word.list <- lapply(stopped.word.list, naomit)
    stopped.word.list <- lapply(stopped.word.list, function(x){ 
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
    freq.word.list <- lapply(word.lists2, COUNT)
    freq.stop.word.list <- lapply(stopped.word.list, COUNT)
    red.freq.stop.word.list <- ncutWORDS(freq.stop.word.list, cut.n = cut.n) 
    word.lists2 <- lapply(word.lists2, function(x) {
            if (is.null(x)){
                return(x)
            } else { 
                comment(x) <- "bagOwords"
                return(x)
            }
        }
    )
    stopped.word.list <- lapply(stopped.word.list, function(x) {
        if (is.null(x)){
                return(x)
        } else { 
                comment(x) <- "bagOwords"
                return(x)
            }
        }
    )
    freq.word.list <- lapply(freq.word.list, function(x) {
            if (is.null(x)) {
                return(x)
            } else { 
                comment(x) <- "freqList"
                return(x)
            }
        }
    )
    freq.stop.word.list <- lapply(freq.stop.word.list, function(x) {
            if (is.null(x)) {
                return(x)
            } else { 
                comment(x) <- "freqList"
                return(x)
            }
        }
    )
    red.freq.stop.word.list <- lapply(red.freq.stop.word.list, function(x) {
            if (is.null(x)){
                return(x)
            } else { 
                comment(x) <- "freqList"
                return(x)
            }
        }
    )
    comment(word.lists2) <- "cwl"    
    comment(stopped.word.list) <- "swl"
    comment(freq.word.list) <- "fwl" 
    comment(freq.stop.word.list) <- "fswl"
    comment(red.freq.stop.word.list) <- "rfswl"
    if (alphabetical) {
        asort <- function(dat, col=1) {
            dat2 <-dat[order(dat[, col]), ]
            rownames(dat2) <- NULL
            return(dat2)
        }
        freq.word.list <- lapply(freq.word.list, asort)
        freq.stop.word.list <- lapply(freq.stop.word.list, asort)
        red.freq.stop.word.list <- lapply(red.freq.stop.word.list, asort)
    } 
    o <- list(cwl = word.lists2, swl = stopped.word.list, 
        fwl = freq.word.list, fswl = freq.stop.word.list, 
        rfswl = red.freq.stop.word.list)
    class(o) <- "word.list"
    return(o)
}

#' Prints a word.list object
#' 
#' Prints a word.list object.
#' 
#' @param x The word.list object
#' @param \ldots ignored
#' @method print word.list
#' @S3method print word.list
print.word.list <-
function(x, ...) {
    print(x$rfswl)
}
