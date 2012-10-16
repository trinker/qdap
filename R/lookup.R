#' Hash Table/ Dictionary Lookup
#'  
#' Useful for large vector lookups
#' 
#' @aliases lookup
#' @param terms a vector of terms to undergo a lookup
#' @param key.match either a two column data frame (if data frame supplied no key reassign needed)  of a match key and reassignment column or a single vector match key.
#' @param key.reassign a single reassingment vector supplied if key.match is not a two column data frame
#' @param missing value to assign to terms not matching the key.match
#' @return Outputs a new vector with reassigned values.
#' @seealso 
#' \code{\link[base]{new.env}}
#' @keywords dictionary hash lookup
#' @examples
#' lookup(mtcars$carb, sort(unique(mtcars$carb)),        
#'     c('one', 'two', 'three', 'four', 'six', 'eight')) 
#' lookup(mtcars$carb, sort(unique(mtcars$carb)),        
#'     seq(10, 60, by=10))
#' 
#' key <- data.frame(x=1:2, y=c("A", "B"))
#' big.vec <- sample(1:2, 3000000, T)
#' lookup(big.vec, key)
#' 
#' lookup(1:5, data.frame(1:4, 11:14))
#' lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))
lookup <-
function(terms, key.match, key.reassign=NULL, missing = NA) {
    hash <- function(x, mode.out) {
        e <- new.env(hash = TRUE, size = nrow(x), 
            parent = emptyenv())
        FUN <- paste0("as.", mode.out)
        FUN <- match.fun(FUN)
        apply(x, 1, function(col) assign(col[1], 
            FUN(col[2]), envir = e))
        return(e)
    }  
    if (is.null(key.reassign)) {
        if (is.factor(key.match[, 2])) {
            key.match[, 2] <- as.character(key.match[, 2])
        }
        mode.out <- mode(key.match[, 2])    
        DF <- key.match
        DF[, 1] <- as.character(DF[, 1])
    } else {
        if (is.factor(key.reassign)) {
            key.reassign <- as.character(key.reassign)
        }        
        mode.out <- mode(key.reassign)    
        DF <- data.frame(as.character(key.match), key.reassign, 
            stringsAsFactors = FALSE)   
    }
    KEY <- hash(DF, mode.out = mode.out)                                                               
    recoder <- function(x, env, missing){                               
        x <- as.character(x) #turn the numbers to character    
        rc <- function(x){                                    
           if(exists(x, env = env))get(x, e = env) else missing     
        }                                                      
        sapply(x, rc, USE.NAMES = FALSE)                       
    }                                                              
    x<- recoder(terms, KEY, missing = missing)     
    return(x)
}
