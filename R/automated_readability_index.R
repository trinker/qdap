#' Readabilitiy Measures
#' 
#' \code{automated_readability_index} - Apply Automated Readability Index to 
#' transcript(s) by zero or more grouping variable(s).
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @return Generates a dataframe with selected readability statistic by grouping 
#' variable(s).  The \code{frey} function returns a graphic representation of 
#' the readability.
#' @rdname Readability
#' @note The Automated Readability Index is a readability score which approximates 
#' the grade level needed to comprehend the text. The Automated Readability Index 
#' is derived from word difficulty (letters per word) and sentence difficulty 
#' (words per sentence).  If you have not run the sentSplit function on your 
#' data the results may not be accurate.
#' @references R. J. Senter and E. A. Smith. (1967) Automated readability index. 
#' Technical Report AMRLTR-66-220, University of Cincinnati, Cincinnati, Ohio.
#' 
#' Coleman, M. & Liau, T. L. (1975). A computer readability formula designed 
#' for machine scoring. Journal of Applied Psychology, Vol. 60, pp. 283-284.
#' 
#' McLaughlin, G. Harry (1969). SMOG Grading - a New Readability Formula. 
#' Journal of Reading, Vol. 12(8), pp. 639-646. 
#' @keywords readability, Automated Readability Index, Coleman Liau, SMOG
#' @export
#' @examples
#' \dontrun{
#' with(rajSPLIT, automated_readability_index(dialogue, list(person, act)))
#' with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))
#' 
#' with(rajSPLIT, coleman_liau(dialogue, list(person, act)))
#' with(rajSPLIT, coleman_liau(dialogue, list(sex, fam.aff)))
#' 
#' with(rajSPLIT, SMOG(dialogue, list(person, act)))
#' with(rajSPLIT, SMOG(dialogue, list(sex, fam.aff)))
#' }
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

#' Coleman Liau Readability
#' 
#' \code{coleman_liau} - Apply Coleman Liau Index to 
#' transcript(s) by zero or more grouping variable(s).
#' 
#' @rdname Readability
#' @param rm.incomplete logical.  If TRUE removes incomplete sentences from the 
#' analysis.
#' @param \ldots Other arguments passed to \code{endf}.
#' @export
coleman_liau <-
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
    DF$word.count <- word.count(DF$text.var, missing = 0, 
                                digit.remove = FALSE)
    i <- as.data.frame(table(DF$group))
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character.count(DF$text.var, apostrophe = FALSE, 
                                          digit.remove = FALSE)
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- aggregate(character.count ~ 
                                       group, DF, sum)$character.count 
    clf <- function(tse, tw, tc) (.0588*((100*tc)/tw)) - 
      (.296*((100*tse)/tw) ) - 15.8
    DF2$Coleman_Liau <- round(with(DF2, clf(tse = sentence.count, 
                                            tc = character.count, tw = word.count)), digits = 1)
    names(DF2) <- c(G, names(DF2)[-1])
    return(DF2)
}


#' SMOG Readability
#' 
#' \code{SMOG} - Apply SMOG to transcript(s) by zero or more grouping variable(s).
#' 
#' @rdname Readability
#' @param output A character vector character string indicating output type. 
#' One of "valid" (default and congruent with McLaughlin's intent) or "all". 
#' @export
SMOG <-
function(text.var, grouping.var = NULL, output = "valid", 
         rm.incomplete = FALSE, ...) {
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
  i <- as.data.frame(table(DF$group))
  DF <- switch(output,
               valid = {subset(DF, group%in%as.character(i[i$Freq > 29, 
                                                           ][,'Var1']))},
               all = DF)
  DF$group <- DF$group[ , drop=TRUE]
  DF$tot.n.sent <- 1:nrow(DF)
  DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
  DF$pollysyllable.count <- pollysyllable.sum(DF$text.var)
  DF2 <- aggregate(word.count ~ group, DF, sum)
  DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
  DF2$pollysyllable.count <- aggregate(pollysyllable.count ~ 
    group, DF, sum)$pollysyllable.count   
  smog <- function(tse, tpsy) 1.043 * sqrt(tpsy * (30/tse)) + 3.1291 
  DF2$SMOG <- round(with(DF2, smog(tse = sentence.count, 
    tpsy = pollysyllable.count)), digits = 1)
  DF2$validity <- ifelse(DF2$sentence.count < 30, "n < 30", 
                         "valid")
  if(output == "valid") DF2$validity <- NULL
  names(DF2) <- c(G, names(DF2)[-1])
  return(DF2)
}
