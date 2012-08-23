#' Stems a vector of text strings
#' 
#' Stems a vector of text strings
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param rm.bracket %% ~~Describe \code{rm.bracket} here~~
#' @param capitalize %% ~~Describe \code{capitalize} here~~
#' @param warn %% ~~Describe \code{warn} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text.var, rm.bracket = TRUE, capitalize = TRUE, warn = TRUE, 
#'     ...) 
#' {
#'     require(tm)
#'     txt <- as.character(text.var)
#'     if (rm.bracket) {
#'         txt <- bracketX(txt)
#'     }
#'     nc <- nchar(txt)
#'     end.mark1 <- substring(txt, nc, nc)
#'     end.mark2 <- substring(txt, nc - 1, nc)
#'     sent.type <- ifelse(end.mark2 %in% c("*.", "*?", "*!", "*|"), 
#'         end.mark2, ifelse(end.mark1 %in% c(".", "?", "!", "|"), 
#'             end.mark1, ifelse(is.na(end.mark1), NA, "")))
#'     bl <- sent.type == ""
#'     if (any(na.omit(bl)) & warn) {
#'         warning(paste("The following row(s) do have standard qdap punctuation endmarks:\n", 
#'             " rows:", which(bl)))
#'     }
#'     LIST <- stopwords(txt, stopwords = NULL, strip = TRUE)
#'     LIST <- lapply(LIST, function(x) ifelse(is.na(x), NA, tm::stemDocument(x)))
#'     if (capitalize) {
#'         LIST <- lapply(LIST, function(x) capitalizer(x, ...))
#'     }
#'     if (length(text.var) == 1) {
#'         if (!is.na(text.var)) {
#'             LIST <- paste2(LIST, sep = " ")
#'         }
#'     }
#'     txt2 <- paste2(list(lapply(LIST, paste2, sep = " "), sent.type), 
#'         sep = "")
#'     if (capitalize) {
#'         capit <- function(string) {
#'             if (is.na(string)) {
#'                 NA
#'             }
#'             else {
#'                 nc <- nchar(string)
#'                 paste0(toupper(substr(string, 1, 1)), substr(string, 
#'                   2, nc))
#'             }
#'         }
#'         txt2 <- unlist(lapply(Trim(txt2), capit))
#'     }
#'     return(txt2)
#'   }
#' 
stemmer <-
function(text.var, rm.bracket = TRUE, capitalize = TRUE, 
    warn = TRUE, ...){
    suppressWarnings(require(tm))
    txt <- as.character(text.var)
    if (rm.bracket){
        txt <- bracketX(txt)
    }
    nc <- nchar(txt)
    end.mark1 <- substring(txt, nc, nc)
    end.mark2 <- substring(txt, nc - 1, nc)
    sent.type <- ifelse(end.mark2 %in% c("*.", "*?", "*!", "*|"), end.mark2,
        ifelse(end.mark1 %in% c(".", "?", "!", "|"), end.mark1,    
        ifelse(is.na(end.mark1), NA, "")))
    bl <- sent.type == ""
    if (any(na.omit(bl)) & warn) {
        warning(paste(
            "The following row(s) do have standard qdap punctuation endmarks:\n", 
            " rows:", which(bl)))
    }
    LIST <- stopwords(txt, stopwords = NULL, strip = TRUE)
    LIST <- lapply(LIST, function(x) {
        if(identical(x, character(0))) {
                NA
            } else {
                tm::stemDocument(x)
            }
        }
    )
    if (capitalize){
        LIST <- lapply(LIST, function(x) capitalizer(x, ...))
    }
    if (length(text.var) == 1) {
        if (!is.na(text.var)) {
            LIST <- paste2(LIST, sep=" ")
        }
    }
    LIST2 <- lapply(LIST, function(x) paste(x, collapse=" "))
    txt2 <- paste2(list(LIST2, sent.type), sep="")
    if (capitalize){
        capit <- function (string) {
            if (string == "NA" | is.na(string)) {
                NA
            } else {
                nc <- nchar(string)
                paste0(toupper(substr(string, 1, 1)), substr(string, 2, nc))
            }
        }
    txt2 <- unlist(lapply(Trim(txt2), capit))
    }
    return(txt2)
}


stem.words <- 
function(...) {
  stemmer(c(...), capitalize = FALSE, warn = FALSE, ...)
}
