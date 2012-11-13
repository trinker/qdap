#' Automated Readability Index
#' 
#' Transcript apply Automated Readability Index
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @return Generates the Automated Readability Index by grouping variable(s)
#' @note The Automated Readability Index is a readability score which approximates the grade level needed to comprehend the text. The Automated Readability Index is derived from word difficulty (letters per word) and sentence difficulty (words per sentence).
#' @warning If you have not run the sentSplit function on your data the results will not be accurate.
#' @seealso
#' \code{\link[qdap]{SMOG}}, 
#' \code{\link[qdap]{flesch_kincaidg}},
#' \code{\link[qdap]{frey}},
#' \code{\link[qdap]{coleman_liau}},
#' \code{\link[qdap]{linsear_write}},
#' \code{\link[qdap]{word_stats}}
#' @references R. J. Senter and E. A. Smith. (1967) Automated readability index. Technical Report AMRLTR-66-220, University of Cincinnati, Cincinnati, Ohio.
#' @keywords readability, Automated Readability Index
#' @examples
#' with(rajSPLIT, automated_readability_index(dialogue, list(person, act)))
#' with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))
automated_readability_index <-
function(text.var, grouping.var = NULL) {
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
    DF$word.count <- word.count(DF$text.var, missing = 0)

    i <- as.data.frame(table(DF$group))

    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character.count(DF$text.var)

    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- aggregate(character.count ~ 
        group, DF, sum)$character.count 
    ari <- function(tse, tw, tc) 4.71*(tc/tw) + .5*(tw/tse) - 21.43
    DF2$Automated_Readability_Index <- round(with(DF2, 
        ari(tse = sentence.count, tc = character.count, tw = word.count)), 
        digits = 1)
    names(DF2) <- c(G, names(DF2)[-1])
    return(DF2)
}
