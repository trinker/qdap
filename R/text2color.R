#' Map Words to Colors
#' 
#' A dictionary lookup that maps words to colors.
#' 
#' @param words A vector of words.
#' @param recode.words A vector of unique words or a list of unique word vectors 
#' that will be matched against corresponding colors.
#' @param colors A vector of colors of equal in length to recode.words + 1(the 
#' +1 is for unmatched words).
#' @return Returns a vector of mapped colors equal in length to the words vector.
#' @seealso \code{\link[qdap]{lookup}}
#' @keywords color, lookup, recode, dictionary
#' @export
#' @examples
#' \dontrun{
#' set.seed(10)
#' x <- data.frame(X1 = sample(Top25Words[1:10], 20, TRUE))
#' 
#' #blue was recycled
#' text2color(x$X1, qcv(the, and, is), qcv(red, green, blue)) 
#' text2color(x$X1, qcv(the, and, is), qcv(red, green, blue, white))
#' x$X2 <- text2color(x$X1, list(qcv(the, and, is), "that"), 
#'     qcv(red, green, white))
#' x
#' }
text2color <-
function(words, recode.words, colors) {
  nc <- length(colors)
  if ((nc -1) != length(recode.words)) {
      warning("length of colors should be 1 more than length of recode.words")
  }
  nomatch <- colors[nc]
  colors <- colors[-nc]
  nulls <- sapply(recode.words, function(x) identical(x, character(0)))
  recode.words[nulls] <- ""
  lookup <- lapply(seq_along(recode.words), function(n) 
    cbind(recode.words[[n]], colors[n]))
  lookup <- do.call("rbind.data.frame", lookup)
  lookup <- apply(lookup, 2, as.character)
  recode <- lookup[match(words, lookup[, 1]), 2]
  recode[is.na(recode)] <- nomatch
  return(recode)
}


