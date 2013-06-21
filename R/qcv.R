#' Quick Character Vector
#' 
#' Create a character vector without the use of quotation marks.
#' 
#' @param \ldots Character objects. Either \ldots or \code{terms} argument must 
#' be utilized.
#' @param terms An optional argument to present the terms as one long character 
#' string.  This is useful if the split (separator) is not a comma (e.g., spaces 
#' are the term separators).
#' @param space.wrap logical.  If \code{TRUE} wraps the vector of terms with a 
#' leading/trailing space. 
#' @param trailing logical.  If \code{TRUE} wraps the vector of terms with a 
#' trailing space. 
#' @param leading logical.  If \code{TRUE} wraps the vector of terms with a 
#' leading space. 
#' @param split Character vector of length one to use for splitting (i.e., the 
#' separator used in the vector).  For use with the argument \code{terms}.
#' @param rm.blank logical.  If \code{TRUE} removes all blank spaces from the 
#' vector.
#' @return Returns a character vector.
#' @seealso \code{\link[base]{c}}
#' @keywords character
#' @export
#' @examples
#' \dontrun{
#' qcv(I, like, dogs)
#' qcv(terms = "I, like, dogs") #default separator is " "
#' qcv(terms = "I, like, dogs", split = ",")
#' qcv(terms = "I  like dogs")
#' qcv(I, like, dogs, space.wrap = TRUE)
#' qcv(I, like, dogs, trailing = TRUE)
#' qcv(I, like, dogs, leading = TRUE)
#' exclude(Top25Words, qcv(the, of, and))
#' qcv(terms = "mpg cyl  disp  hp drat    wt  qsec vs am gear carb")
#' }
qcv <- 
function(..., terms = NULL, space.wrap = FALSE, trailing = FALSE, 
    leading = FALSE, split = " ", rm.blank = TRUE){
    if (!is.null(terms)) {
        x <- strsplit(terms, split = split)
    } else {
        x <- substitute(...())
    }
    z <- unblanker(scrubber(unlist(lapply(x, function(y) as.character(y)))))
    if (rm.blank) {
        z <- unblanker(z)
    }
    if (space.wrap){
        z <- spaste(z)
    }      
    z <- spaste(z, trailing = trailing, leading = leading) 
    z
}
