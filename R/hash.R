#' Hash/Dictionary Lookup
#' 
#' Creates a new environemnt for quick hash style dictionary lookup
#' 
#' @param A two column dataframe
#' @return Creates a "hash table" or a two column data frame in its own evironment.  
#' @author Bryan Goodrich and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso \code{\link[qdap]{lookup}},
#' \code{\link[base]{environment}}
#' @references \url{http://www.talkstats.com/showthread.php/22754-Create-a-fast-dictionary}
#' @keywords hash, dictionary, lookup
#' @export
#' @examples
#' \dontrun{
#' new.hash <- hash(aggregate(conc~Plant, CO2, sum))
#' new.hash <- hash(aggregate(conc~Plant, CO2, sum))
#' sapply(as.character(CO2$Plant), function(x) {
#'         if(exists(x, env = new.hash)) {
#'             get(x, e = new.hash) 
#'         } else {
#'             NA
#'         }
#'     }
#' )
#' }
hash <-
function(x) {
    e <- new.env(hash = TRUE, size = nrow(x), 
        parent = emptyenv())
    apply(x, 1, function(col) assign(col[1], 
        as.numeric(col[2]), envir = e))
    return(e)
}
