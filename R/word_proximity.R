#' Proximity Matrix Between Words
#' 
#' Generate proximity measures to ascertain a mean distance measure between 
#' word uses.
#' 
#' @param text.var The text variable.
#' @param terms  A vector of quoted terms.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @return Returns a matrix of proximity measures in the unit of average 
#' sentences between words.
#' @note The match.terms is character sensitive.  Spacing is an important way 
#' to grab specific words and requires careful thought.  Using "read" will find 
#' the words "bread", "read" "reading", and "ready".  If you want to search 
#' for just the word "read" you'd supply a vector of c(" read ", " reads", 
#' " reading", " reader").  
#' @export
#' @examples
#' \dontrun{
#' wrds <- word_list(pres_debates2012$dialogue, 
#'     stopwords = c("it's", "that's", Top200Words))
#' wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
#' 
#' (x <- with(pres_debates2012, word_proximity(dialogue, wrds2)))
#' out <- round(scale(x), 2)
#' diag(out) <- attr(out, "scaled:center")
#' qheat(scale(x), values=TRUE, digits=2, high="red", low="yellow", grid=NULL)
#' }
word_proximity <- function(text.var, terms, grouping.var = NULL) {
 
## check the grep use
## describe the non-symetrical matrix (look at cm_distance)
## pvals???
## plot method
  
    if(!is.null(grouping.var)){
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    inds <- seq_along(terms)
    ncols <- max(inds)
    mat <- matrix(rep(NA, ncols^2), ncols)
    terms <- tolower(terms)
    colnames(mat) <- rownames(mat) <- terms

    if (is.null(grouping.var)) {
        word_proximity_helper(text.var, terms, tofill = mat, inds = inds)
    } else {
        splits <- split(text.var, grouping)
        suppressWarnings(lapply(splits, word_proximity_helper, terms = terms, tofill = mat, inds = inds))
    }
}

word_proximity_helper <- function(text.var, terms, tofill, inds) {

    text.var <- spaste(strip(na.omit(sent_detect(text.var)), 
         apostrophe.remove = FALSE))

    for (i in inds){
        for (j in inds){
            tofill[i, j] <- ave_prox(terms[i], terms[j], text.var)
        }   
    }
    tofill
}

ave_prox <- function(x, y, text.var, ignore.case = TRUE) {
     if (x == y) return(NA)

     locsx <- grepl(x, text.var)
     locsy <- grepl(y, text.var)
     mean(min_dist(locsx, locsy))
}

min_dist <- function(x, y) {
    unlist(lapply(which(x), function(x) {
        min(abs(x - which(y)))
    }))
}


