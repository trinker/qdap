#' Hash/Dictionary Lookup
#' 
#' Creates a new environment for quick hash style dictionary lookup.
#' 
#' @param x A two column dataframe.
#' @param mode.out The type of output (column 2) expected (e.g. \code{"character"}, 
#' \code{"numeric"}, etc.)
#' @return Creates a "hash table" or a two column data frame in its own 
#' environment.  
#' @author Bryan Goodrich and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso \code{\link[qdap]{lookup}},
#' \code{\link[base]{environment}}
#' @references \url{http://www.talkstats.com/showthread.php/22754-Create-a-fast-dictionary}
#' @keywords hash, dictionary, lookup
#' @export
#' @examples
#' \dontrun{
#' (DF <- aggregate(mpg~as.character(carb), mtcars, mean))
#' 
#' new.hash <- hash(DF)
#' sapply(as.character(mtcars$carb), function(x) {
#'         if(exists(x, env = new.hash)) {
#'             get(x, e = new.hash)
#'         } else {
#'             NA
#'         }
#'     }
#' )
#' 
#' new.hash <- hash(DF, "character")
#' sapply(as.character(mtcars$carb), function(x) {
#'         if(exists(x, env = new.hash)) {
#'             get(x, e = new.hash)
#'         } else {
#'             NA
#'         }
#'     }
#' )
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
        return(evnt)
    }  
    hash2(x, mode.out = mode.out)                                                                   
}
