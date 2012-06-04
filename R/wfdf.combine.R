#' Aggregate a wfm or wfdf Object By Supplied Word List(s)
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param wfdf %% ~~Describe \code{wfdf} here~~
#' @param word.lists %% ~~Describe \code{word.lists} here~~
#' @param matrix %% ~~Describe \code{matrix} here~~
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
#' function (wfdf, word.lists, matrix = FALSE) 
#' {
#'     suppressWarnings(if (is.list(word.lists) & length(word.lists) > 
#'         1 & any(Reduce("%in%", word.lists))) {
#'         stop("overlapping words in word.lists")
#'     })
#'     if (comment(wfdf) == "t.df") {
#'         wfdf <- wfdf
#'     }
#'     else {
#'         if (comment(wfdf) %in% c("true.matrix", "m.df")) {
#'             wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
#'         }
#'         else {
#'             stop("Object must be a raw word frequency data frame")
#'         }
#'     }
#'     if (is.list(word.lists) & is.null(names(word.lists))) {
#'         NAMES <- paste("words", 1:length(word.lists))
#'     }
#'     else {
#'         if (is.list(word.lists) & !is.null(names(word.lists))) {
#'             NAMES <- names(word.lists)
#'         }
#'         else {
#'             if (is.vector(word.lists)) {
#'                 G <- as.character(substitute(word.lists))
#'                 if (G[1] == "c") {
#'                   NAMES <- "words"
#'                 }
#'                 else {
#'                   NAMES <- G[length(G)]
#'                 }
#'             }
#'             else {
#'                 stop("incorrect word.list argument")
#'             }
#'         }
#'     }
#'     if (!is.list(word.lists)) 
#'         word.lists <- list(word.lists)
#'     j <- lapply(word.lists, function(x) wfdf[wfdf[, 1] %in% x, 
#'         -1])
#'     if (!all(wfdf[, 1] %in% unlist(word.lists))) {
#'         j[[length(j) + 1]] <- wfdf[!wfdf[, 1] %in% unlist(word.lists), 
#'             -1]
#'     }
#'     k <- lapply(j, function(x) if (is.vector(x)) {
#'         x
#'     }
#'     else {
#'         colSums(x)
#'     })
#'     m <- do.call("rbind", k)
#'     rownames(m) <- 1:nrow(m)
#'     NAMES <- if (all(wfdf[, 1] %in% unlist(word.lists))) {
#'         NAMES
#'     }
#'     else {
#'         c(NAMES, "else.words")
#'     }
#'     DFF <- data.frame(word.group = NAMES, m)
#'     if (matrix) {
#'         DFF2 <- as.matrix(DFF[, -1])
#'         rownames(DFF2) <- as.character(DFF[, 1])
#'         DFF <- DFF2
#'     }
#'     comment(DFF) <- ifelse(!matrix, "t.df", "true.matrix")
#'     return(DFF)
#'   }
#' 
wfdf.combine <-
function(wfdf, word.lists, matrix = FALSE){
    suppressWarnings(if (is.list(word.lists) & length(word.lists) > 1 & 
        any(Reduce("%in%", word.lists))) {
        stop("overlapping words in word.lists")
    })
    if (comment(wfdf) == "t.df") {
        wfdf <- wfdf
    } else {
        if (comment(wfdf) %in% c("true.matrix", "m.df")) { 
            wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
        } else {
            stop("Object must be a raw word frequency data frame")
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
    j <- lapply(word.lists, function(x) wfdf[wfdf[, 1] %in% x, -1])
    if (!all(wfdf[, 1] %in% unlist(word.lists))) {
        j[[length(j) + 1]] <- wfdf[!wfdf[, 1] %in% unlist(word.lists), -1]
    }
    k <- lapply(j, function(x) if(is.vector(x)) { x } else { colSums(x)})
    m <- do.call("rbind", k)
    rownames(m) <- 1:nrow(m)
    NAMES <- if (all(wfdf[, 1] %in% unlist(word.lists))) {
        NAMES 
    } else {
        c(NAMES, "else.words")
    }
    DFF <- data.frame(word.group = NAMES, m)
    if (matrix) {
        DFF2 <- as.matrix(DFF[, -1])
        rownames(DFF2) <- as.character(DFF[, 1])
        DFF <- DFF2
    }
    comment(DFF) <- ifelse(!matrix, "t.df", "true.matrix")
    return(DFF)
}
