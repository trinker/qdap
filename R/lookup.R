#' Hash Table/Dictionary Lookup
#'  
#' Environment based hash table useful for large vector lookups.
#' 
#' @param terms A vector of terms to undergo a lookup.
#' @param key.match Takes one of the following: (1) a two column data.frame of a 
#' match key and reassignment column, (2) a named list of vectors (Note: if 
#' data.frame or named list supplied no key reassign needed) or (3) a single 
#' vector match key.
#' @param key.reassign A single reassignment vector supplied if key.match is 
#' not a two column data.frame/named list.
#' @param missing Value to assign to terms not matching the key.match.  If set 
#' to \code{NULL} the original values in \code{terms} corresponding to the 
#' missing elements are retained.
#' @return Outputs A new vector with reassigned values.
#' @seealso 
#' \code{\link[base]{new.env}}
#' @keywords dictionary, hash, lookup
#' @export
#' @rdname lookup
#' @examples
#' \dontrun{
#' ## Supply a dataframe to key.match
#' 
#' lookup(1:5, data.frame(1:4, 11:14))
#' 
#' ## Retain original values for missing 
#' lookup(1:5, data.frame(1:4, 11:14), missing=NULL) 
#' 
#' lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))
#' 
#' key <- data.frame(x=1:2, y=c("A", "B"))
#' big.vec <- sample(1:2, 3000000, T)
#' out <- lookup(big.vec, key)
#' out[1:20]
#' 
#' ## Supply a named list of vectors to key.match
#' 
#' codes <- list(A=c(1, 2, 4), 
#'     B = c(3, 5),
#'     C = 7,
#'     D = c(6, 8:10))
#' 
#' lookup(1:10, codes)
#' 
#' ## Supply a single vector to key.match and key.assign
#' 
#' lookup(mtcars$carb, sort(unique(mtcars$carb)),        
#'     c('one', 'two', 'three', 'four', 'six', 'eight')) 
#'     
#' lookup(mtcars$carb, sort(unique(mtcars$carb)),        
#'     seq(10, 60, by=10))
#'     
#' ## %l%, a binarary operator version of lookup
#' 1:5 %l% data.frame(1:4, 11:14)
#' 1:10 %l% codes
#' }
lookup <-
function(terms, key.match, key.reassign=NULL, missing = NA) {
    hash <- function(x, mode.out) {
        e <- new.env(hash = TRUE, size = nrow(x), 
            parent = emptyenv())
        FUN <- paste0("as.", mode.out)
        FUN <- match.fun(FUN)
        apply(x, 1, function(col) {
            assign(col[1], FUN(col[2]), envir = e)
        })
        return(e)
    }  
    if (is.matrix(key.match)) {
        key.match <- data.frame(key.match)
    }
    if (is.list(key.match)) {
        if (!is.data.frame(key.match)) {
            key.match <- list2df(key.match) 
        }
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
    recoder <- function(x, envr, missing){                               
        x <- as.character(x) #turn the numbers to character    
        rc <- function(x, envr){                                    
            if(exists(x, envir = envr)) {
                get(x, envir = envr) 
            } else {
                missing     
            }
        }                                                      
        sapply(x, rc, USE.NAMES = FALSE, envr = envr)                       
    }    
    if (is.null(missing)) {
        x <- recoder(terms, envr = KEY, missing = NA) 
        x[is.na(x)] <- terms[is.na(x)]
        x
    } else {                                                         
        recoder(terms, envr = KEY, missing = missing)     
    }
}


#' Hash/Dictionary Lookup
#' 
#' \code{terms \%l\% key.match} - A binary operator version of \code{lookup} 
#' for when \code{key.match} is a data.frame or named list.
#'
#' @export
#' @rdname lookup
`%l%` <- function(terms, key.match) lookup(terms = terms, key.match = key.match)
