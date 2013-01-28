#' Paste an Unspecified Number Of Text Columns
#' 
#' Paste unspecified columns or a list of vectors together.
#' 
#' @param multi.columns The multiple columns or a list of vectors to paste 
#' together.
#' @param sep A character string to separate the terms. 
#' @param handle.na logical.  If TRUE returns \code{NA} if any column/vector 
#' contains a missing value.
#' @param trim logical.  If TRUE leading/trailing white space is removed.
#' @return Returns a vector with row-wise elements pasted together.
#' @note \code{\link[base]{paste}} differs from \code{\link[qdap]{paste2}} 
#' because \code{paste} does not allowed an unspecified number of columns to be 
#' pasted.  This behavior can be convenient for inside of functions when the 
#' number of columns being pasted is unknown.
#' @seealso \code{\link[base]{paste}}
#' @keywords paste
#' @export
#' @examples
#' \dontrun{
#' v <- rep(list(state.abb[1:8],  month.abb[1:8]) , 5)
#' n <- sample(5:10, 1)
#' paste(v[1:n]) #odd looking return
#' paste2(v[1:n]) 
#' paste2(v[1:n], sep="|") 
#' paste2(mtcars[1:10,], sep="|") 
#' paste(mtcars[1:10,], sep="|") #odd looking return
#' paste2(CO2[1:10,], sep="|-|") 
#' }
paste2 <-
function(multi.columns, sep=".", handle.na=TRUE, trim=TRUE){
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns)
    }
    if (trim) multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        }
    )
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call('cbind', multi.columns)
    } 
    m <- if (handle.na){
                 apply(multi.columns, 1, function(x){
                     if (any(is.na(x))){
                         NA
                     } else {
                         paste(x, collapse = sep)
                     }
                 }
             )   
         } else {
             apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}
