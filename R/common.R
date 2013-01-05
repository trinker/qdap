#' Find Common Words Between Groups
#' 
#' Find common words between grouping variables (e.g. people).
#' 
#' @param word.list A list of names chacter vectors.
#' @param overlap Minimum/exact amount of overlap.
#' @param equal.or A character vector of c(\code{"equal"}, \code{"greater"}, 
#' \code{"more"}, \code{"less"}).
#' @param \dots In liu of word.list the user may input n  number of character 
#' vectors.
#' @rdname Common Words
#' @return Returns a dataframe of all words that match the criteria set by 
#' \code{overlap} and \code{equal.or}.
#' @export
#' @examples
#' a <- c("a", "cat", "dog", "the", "the")                                                              
#' b <- c("corn", "a", "chicken", "the")                                                                
#' d <- c("house", "feed", "a", "the", "chicken")                                                       
#' common(a, b, d, overlap=2)  
#' common(a, b, d, overlap=3)                                                                          
#'                                                                                                      
#' r <- list(a, b, d)  
#' common(r)                                                                                 
#' common(r, overlap=2)                                                                                            
#'                                                                                                     
#' common(word_list(DATA$state, DATA$person)$cwl, overlap = 2)  
common <-
  function(x, ...){
    UseMethod("common")
}

#' @return \code{NULL}
#'
#' @rdname Common Words
#' @method common default
#' @S3method common default  
common.default <-
  function(..., overlap = "all", equal.or = "equal"){
    LIS <- list(...)
    return(common.list(LIS, overlap, equal.or))
}

#' @return \code{NULL}
#'
#' @rdname Common Words
#' @method common list
#' @S3method common list
common.list <-
  function(word.list, overlap = "all", equal.or = "more"){
      if(overlap=="all") {
        OL <- length(word.list) 
      } else {
        OL <- overlap
      }
    LIS <- sapply(word.list, unique)
    DF <- as.data.frame(table(unlist(LIS)),  stringsAsFactors = FALSE)
    names(DF) <- c("word", "freq")
    DF <- DF[order(-DF$freq, DF$word), ]
    DF <- switch(equal.or,
                 equal = DF[DF$freq == OL, ],
                 greater = DF[DF$freq > (OL - 1), ],
                 more = DF[DF$freq > (OL - 1), ],
                 less = DF[DF$freq < (OL + 1), ])
    rownames(DF) <- 1:nrow(DF)
    return(DF)
}



