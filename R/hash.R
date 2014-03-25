#' Hash/Dictionary Lookup
#' 
#' \code{hash} - Creates a new environment for quick hash style dictionary lookup.
#' 
#' @param x A two column dataframe.
#' @param mode.out The type of output (column 2) expected (e.g.,
#' \code{"character"}, \code{"numeric"}, etc.)
#' @param terms A vector of terms to undergo a lookup.
#' @param envir The hash environment to use.
#' @param missing Value to assign to terms not found in the hash table.
#' @return Creates a "hash table", a two column data frame in its own 
#' environment.  
#' @author Bryan Goodrich and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso \code{\link[qdap]{lookup}},
#' \code{\link[base]{environment}}
#' @references \url{http://www.talkstats.com/showthread.php/22754-Create-a-fast-dictionary}
#' @keywords hash, dictionary, lookup
#' @rdname hash
#' @export
#' @examples
#' \dontrun{
#' (DF <- aggregate(mpg~as.character(carb), mtcars, mean))
#' new.hash1 <- hash(DF)  #numeric outcome
#' x <- sample(DF[, 1], 20, TRUE)
#' hash_look(x, new.hash1)
#' 
#' new.hash2 <- hash(DF, "character") #character outcome
#' x %ha% new.hash2
#' }
hash <- 
function(x, mode.out = "numeric") {
    hash2 <- function(x, mode.out) {
        evnt <- new.env(hash = TRUE, size = nrow(x), 
            parent = emptyenv())
        FUN <- paste0("as.", mode.out)
        FUN <- match.fun(FUN)
        apply(x, 1, function(col) {
            assign(col[1], FUN(col[2]), envir = evnt)
        })
        evnt
    }  
    hash2(x, mode.out = mode.out)                                                                   
}

#' Hash/Dictionary Lookup
#' 
#' \code{hash_look} - Works with a hash table such as is returned from 
#' \code{hash}, to lookup values.
#' 
#' @export
#' @rdname hash
hash_look <- function(terms, envir, missing = NA) {
    unlist(lapply(as.character(terms), function(x) {
        if (exists(x, envir = envir)) {
            get(x, envir = envir)
        } else {
            missing
        }
    }))
}

#' Hash/Dictionary Lookup
#' 
#' \code{\%ha\%} - A binary operator version of \code{hash_look}.
#'
#' @export
#' @rdname hash
`%ha%` <- function(terms, envir) hash_look(terms = terms, envir = envir)
