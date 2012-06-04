#' Helper Function for qda: Capitalizes Given Word Lists
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
#' @param caps.list %% ~~Describe \code{caps.list} here~~
#' @param I.list %% ~~Describe \code{I.list} here~~
#' @param no.apostrophe %% ~~Describe \code{no.apostrophe} here~~
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
#' function (text, caps.list = NULL, I.list = TRUE, no.apostrophe = FALSE) 
#' {
#'     I_list <- c("I'm", "I'll", "I'd", "I've", "I")
#'     IDF <- data.frame(from1 = sapply(I_list, function(x) strip(x, 
#'         apostrophe.remove = TRUE)), from2 = sapply(I_list, strip), 
#'         to = I_list)
#'     idf <- if (no.apostrophe == FALSE) {
#'         IDF[-1]
#'     }
#'     else {
#'         IDF2 <- IDF[-2]
#'         names(IDF2) <- c("from2", "to")
#'         data.frame(rbind(IDF[-1], IDF2))
#'     }
#'     names(idf) <- c("from", "to")
#'     rownames(idf) <- 1:nrow(idf)
#'     idf <- if (I.list) 
#'         idf
#'     else NULL
#'     names <- data.frame(from = tolower(caps.list), to = gsub("(\w)(\w*)", 
#'         "\U\1\L\2", tolower(caps.list), perl = T))
#'     names2 <- data.frame(from = paste(names$from, "'s", sep = ""), 
#'         to = paste(names$to, "'s", sep = ""))
#'     idf <- rbind(idf, names, names2)
#'     idf$from <- as.character(idf$from)
#'     idf$to <- as.character(idf$to)
#'     subber <- function(x) ifelse(x %in% idf$from, idf[match(x, 
#'         idf$from), "to"], x)
#'     unlist(lapply(text, subber))
#'   }
#' 
capitalizer <-
function(text, caps.list = NULL, I.list = TRUE, 
    no.apostrophe = FALSE) {
    I_list <- c("I'm", "I'll", "I'd", "I've", "I")
    IDF <- data.frame(from1 = sapply(I_list, function(x) strip(x, 
        apostrophe.remove = TRUE)), from2 = sapply(I_list, strip), 
        to = I_list)
    
    idf <- if (no.apostrophe == FALSE) {
        IDF[-1]
    } else {
        IDF2 <- IDF[-2]
        names(IDF2) <- c("from2", "to")
        data.frame(rbind(IDF[-1], IDF2))
    }
    names(idf) <- c("from", "to")
    rownames(idf) <- 1:nrow(idf)
    
    idf <- if (I.list) 
        idf else NULL
    names <- data.frame(from = tolower(caps.list), to = gsub("(\\w)(\\w*)", 
        "\\U\\1\\L\\2", tolower(caps.list), perl = T))
    names2 <- data.frame(from = paste(names$from, "'s", sep = ""), 
        to = paste(names$to, "'s", sep = ""))
    idf <- rbind(idf, names, names2)
    idf$from <- as.character(idf$from)
    idf$to <- as.character(idf$to)
    subber <- function(x) ifelse(x %in% idf$from, idf[match(x, 
        idf$from), "to"], x)
    unlist(lapply(text, subber))
}
