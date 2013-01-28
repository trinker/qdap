#' Replace Missing Values (NA)
#' 
#' Replace missing values (\code{NA}) in a vector or dataframe.
#' 
#' @param x A vector or dataframe with missing values (\code{NA}).
#' @param replace The value to replace missing values (\code{NA}) with.
#' @return Returns a vector or dataframe with missing values replaced.
#' @keywords missing-value
#' @export
#' @examples
#' \dontrun{
#' set.seed(10)              
#' (x <- sample(c(rep(NA, 4), 1:10), 20, rep=T))
#' NAer(x)
#' 
#' set.seed(10)
#' (y <- data.frame(matrix(x, 5, 4))                           )
#' NAer(y)
#' NAer(y, "MISSING")  
#' }
NAer <-
function(x, replace=0){
    if (is.vector(x)){
          x[is.na(x)] <- replace
          return(x )
    } else {
          y <- apply(x, 1, function(x) {x[is.na(x)] <- replace; return(x)})
          y <- data.frame(t(y), check.names = FALSE)
          return(y)
    }
}
