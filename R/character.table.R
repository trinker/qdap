#' Table of Character Counts
#' 
#' Computes a table of character counts by grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @return Returns a dataframe of character counts by grouping variable.
#' @seealso \code[qdap]{\link{character.count}}
#' @keywords character, count
#' @export
#' @examples
#' char.table(DATA$state, DATA$person)
#' char.table(DATA$state, list(DATA$sex, DATA$adult))
#' colsplit2df(char.table(DATA$state, list(DATA$sex, DATA$adult)))
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
       names(new)[1:length(nots)] <- nots
       new[order(names(new))]
    }))
    DF2 <- data.frame(x = rownames(L3), L3, check.names=FALSE, 
        row.names = NULL)
    colnames(DF2)[1] <- G
    DF2
}
