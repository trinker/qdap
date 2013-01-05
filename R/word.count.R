#' Word Counts
#' 
#' \code{word.count} - Transcript Apply Word Counts
#' 
#' @rdname word.count
#' @param text.var The text variable
#' @param byrow logical.  If TRUE counts by row, if FALSE counts all words.
#' @param missing Value to insert for missing values (empty cells).
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @param names logical.  If TRUE the sentences are given as the names of the 
#' counts.
#' @param apostrophe.remove = TRUE logical.  If TRUE apostrophes will be counted 
#' in the character count.
#' @param count.space logical.  If TRUE spaces are counted as characters.
#' @return word.count returns a word count by row or total.
#' @note wc is a convienent short hand for word.count.
#' @seealso \code{\link[qdap]{syllable.count}}
#' @keywords word-count, character-count
#' @export 
#' @examples
#' # WORD COUNT
#' word.count(DATA$state)
#' wc(DATA$state)
#' word.count(DATA$state, names = TRUE)
#' word.count(DATA$state, byrow=FALSE, names = TRUE)
#' sum(word.count(DATA$state))
#' 
#' # CHARACTER COUNTS
#' character.count(DATA$state)
#' character.count(DATA$state, byrow=FALSE)
#' sum(character.count(DATA$state))
#' 
#' # CHARACTER TABLE
#' character.table(DATA$state, DATA$person)
#' char.table(DATA$state, DATA$person)
#' character.table(DATA$state, list(DATA$sex, DATA$adult))
#' colsplit2df(character.table(DATA$state, list(DATA$sex, DATA$adult)))
word.count <- 
function(text.var, byrow = TRUE, missing = NA, digit.remove = TRUE, names = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | len == 1 && (is.na(x) | is.null(x)), missing, len)
    }
    txt <- stopwords(text.var, strip = TRUE,  digit.remove = digit.remove, 
        stopwords = NULL)
    z <- sapply(txt, len2, missing = missing)
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    if(names) {
        names(z) <- text.var
    }
    z
}

#' @rdname word.count
#' @export
wc <- word.count

#' Count Number of Characters
#' 
#' \code{character.count} - Transcript Apply Character Counts
#' 
#' @return character.count returns a character count by row or total.
#' @rdname word.count
#' @export
character.count <- 
function(text.var, byrow = TRUE, missing = NA, apostrophe.remove = TRUE,
    digit.remove = TRUE, count.space = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | (is.na(x) | is.null(x)), missing, nchar(x))
    }
    txt <- stopwords(text.var, strip = TRUE,  separate =  FALSE,
        digit.remove = digit.remove, stopwords = NULL)
    txt[txt %in% c("", "NA")] <- NA
    if (!count.space) {
        txt <- gsub("\\s+", "", txt)
    }
    z <- unlist(lapply(txt, len2, missing = missing))
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    z
}

#' Table of Character Counts
#' 
#' \code{character.table} - Computes a table of character counts by grouping 
#' variable(s).
#' 
#' @return character.table returns a dataframe of character counts by grouping 
#' variable.
#' @rdname word.count
#' @export
character.table <- function(text.var, grouping.var) {
    G <- if(is.null(grouping.var)) {                                                 
             gv <- TRUE                                                              
             "all"                                                                   
         } else {                                                                    
             gv <- FALSE                                                             
             if (is.list(grouping.var)) {                                            
                 m <- unlist(as.character(substitute(grouping.var))[-1])             
                 m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) x[length(x)]) 
                     paste(m, collapse="&")                                          
             } else {                                                                
                  G <- as.character(substitute(grouping.var))                        
                  G[length(G)]                                                       
             }                                                                       
         }                                                                           
    grouping.var <- if(is.null(grouping.var)){                                       
        rep("all", length(text.var))                                                 
    } else {                                                                         
    if(is.list(grouping.var) & length(grouping.var)>1) {                             
         apply(data.frame(grouping.var), 1, function(x){                             
                     if (any(is.na(x))){                                             
                         NA                                                          
                     }else{                                                          
                         paste(x, collapse = ".")                                    
                     }                                                               
                 }                                                                   
             )                                                                       
        } else {                                                                     
            unlist(grouping.var)                                                     
        }                                                                            
    }                        
    ctab <- function(x) {
        table(unlist(strsplit(tolower(scrubber(paste2(x))), NULL)))
    }
    text.var <- as.character(text.var)
    DF <- data.frame(grouping.var, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF$grouping.var <- factor(DF$grouping.var)
    L1 <- split(DF$text.var, DF$grouping)
    L2 <- lapply(L1, ctab)
    chars <- sort(unique(unlist(lapply(L2, names))))
    L3 <- do.call(rbind, lapply(L2, function(x){
       nots <- chars[!chars %in% names(x)]
       new <- rev(c(x, rep(0, length(nots))))
       if (!identical(nots, character(0))) {
           names(new)[1:length(nots)] <- nots
       }
       new[order(names(new))]
    }))
    DF2 <- data.frame(x = rownames(L3), L3, check.names=FALSE, 
        row.names = NULL)
    colnames(DF2)[1] <- G
    DF2
}

#' @rdname word.count
#' @export
char.table <- character.table