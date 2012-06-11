#' Transcript Apply Flesch Kincaide Readability Statistic
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
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
#' function (text.var, grouping.var = NULL) 
#' {
#'     G <- if (is.null(grouping.var)) {
#'         "all"
#'     }
#'     else {
#'         if (is.list(grouping.var)) {
#'             m <- unlist(as.character(substitute(grouping.var))[-1])
#'             m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
#'             paste(m, collapse = "&")
#'         }
#'         else {
#'             G <- as.character(substitute(grouping.var))
#'             G[length(G)]
#'         }
#'     }
#'     grouping <- if (is.null(grouping.var)) {
#'         rep("all", length(text.var))
#'     }
#'     else {
#'         if (is.list(grouping.var) & length(grouping.var) > 1) {
#'             apply(data.frame(grouping.var), 1, function(x) {
#'                 if (any(is.na(x))) {
#'                   NA
#'                 }
#'                 else {
#'                   paste(x, collapse = ".")
#'                 }
#'             })
#'         }
#'         else {
#'             unlist(grouping.var)
#'         }
#'     }
#'     text <- as.character(text.var)
#'     DF <- na.omit(data.frame(group = grouping, text.var = text, 
#'         stringsAsFactors = FALSE))
#'     DF$word.count <- word.count(DF$text.var, missing = 0)
#'     DF$syllable.count <- syllable.sum(DF$text.var)
#'     DF$tot.n.sent <- 1:nrow(DF)
#'     DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
#'     DF2 <- aggregate(word.count ~ group, DF, sum)
#'     DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
#'     DF2$syllable.count <- aggregate(syllable.count ~ group, DF, 
#'         sum)$syllable.count
#'     fkgl <- function(tw, tse, tsy) 0.39 * (tw/tse) + 11.8 * (tsy/tw) - 
#'         15.59
#'     fre <- function(tw, tse, tsy) 206.835 - 1.015 * (tw/tse) - 
#'         84.6 * (tsy/tw)
#'     DF2$FK_grd.lvl <- round(with(DF2, fkgl(tw = word.count, tse = sentence.count, 
#'         tsy = syllable.count)), digits = 1)
#'     DF2$FK_read.ease <- round(with(DF2, fre(tw = word.count, 
#'         tse = sentence.count, tsy = syllable.count)), digits = 3)
#'     names(DF2) <- c(G, names(DF2)[-1])
#'     return(DF2)
#'   }
#' 
flesch_kincaid <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
    G <- if(is.null(grouping.var)) {
             "all"
         } else {
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), 
                     function(x) x[length(x)])
                 paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    grouping <- if(is.null(grouping.var)){
                     rep("all", length(text.var))
                 } else {
                     if(is.list(grouping.var) & length(grouping.var)>1) {
                         apply(data.frame(grouping.var), 1, function(x){
                             if(any(is.na(x))){NA}else{paste(x, collapse = ".")
                                 }
                             }
                         )
                     } else {
                        unlist(grouping.var)
                     } 
                 } 
    text <- as.character(text.var)
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- endf(dataframe = DF, text.var = text.var, ...)
    }
    DF$word.count <- word.count(DF$text.var, missing = 0)
    DF$syllable.count <- syllable.sum(DF$text.var)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$syllable.count <- aggregate(syllable.count ~ group, DF, 
        sum)$syllable.count  
    fkgl <- function(tw, tse, tsy) 0.39 * (tw/tse) + 11.8 * (tsy/tw) - 15.59
    fre <- function(tw, tse, tsy) 206.835 - 1.015 * (tw/tse) - 84.6 * (tsy/tw)
    DF2$FK_grd.lvl <- round(with(DF2, fkgl(tw = word.count, 
        tse = sentence.count, tsy = syllable.count)), digits = 1)
    DF2$FK_read.ease <- round(with(DF2, fre(tw = word.count, 
        tse = sentence.count, tsy = syllable.count)), digits = 3)
    names(DF2) <- c(G, names(DF2)[-1])
    return(DF2)
}
