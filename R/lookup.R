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
